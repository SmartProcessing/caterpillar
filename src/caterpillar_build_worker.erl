-module(caterpillar_build_worker).

-include_lib("caterpillar.hrl").
-include_lib("caterpillar_internal.hrl").

-behaviour(gen_server).

-define(CU, caterpillar_utils).
-define(CPU, caterpillar_pkg_utils).


-record(state, {
    build_plugins       :: [{atom(), list()}],
    platform_plugins    :: [{atom(), list()}],
    build_path          :: list(),
    buckets             :: reference(),
    deps                :: reference()
}).

-export([start_link/1, start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

start_link(Settings) ->
    gen_server:start_link(?MODULE, Settings, []).

init(Settings) ->
    error_logger:info_msg("initialized build worker: ~p~n", [Settings]),
    BuildPlugins = ?GV(
        build_plugins, 
        Settings, 
        [{deb, caterpillar_deb_plugin}]
    ),
    PlatformPlugins = ?GV(
        platform_plugins, 
        Settings, 
        [{"default", caterpillar_default_builder}]
    ),
    BuildPath = ?GV(
        build_path, 
        Settings, 
        ?DEFAULT_BUILD_PATH 
    ),
    {ok, BuildBuckets} = dets:open_file(buckets,
        [{file, ?GV(buckets, Settings, ?DEFAULT_BUCKETS_DETS)}]),
    {ok, Deps} = dets:open_file(deps,
        [{file, ?GV(deps, Settings, ?DEFAULT_DEPENDENCIES_DETS)}]),
    {ok, #state{
        build_plugins=BuildPlugins,
        platform_plugins=PlatformPlugins,
        build_path=BuildPath,
        buckets=BuildBuckets,
        deps=Deps
    }}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({build, ToBuild}, State) ->
    build_rev(ToBuild, State),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec build_rev(#rev_def{}, #state{}) -> {ok, term()}.  
build_rev(ToBuild, State) -> 
    error_logger:info_msg("received build request: ~p", [ToBuild]),
    PlatformPlugins = State#state.platform_plugins,
    BuildPlugins = State#state.build_plugins,
    BuildPath = State#state.build_path,
    BuildBuckets = State#state.buckets,
    DepsDets = State#state.deps,
    Funs = [
        {fun unpack_rev/2, {BuildPath, BuildBuckets, DepsDets}},
        {fun platform_clean/2, PlatformPlugins},
        {fun platform_test/2, PlatformPlugins},
        {fun platform_clean/2, PlatformPlugins},
        {fun platform_prebuild/2, PlatformPlugins},
        {fun build_prepare/2, BuildPlugins},
        {fun build_check/2, BuildPlugins},
        {fun build_submit/2, BuildPlugins}
    ],
    case catch caterpillar_utils:build_pipe(Funs, {none, ToBuild}) of
        {ok, {Fd, Name}} ->
            ok = gen_server:call(caterpillar, 
                {built, self(), ToBuild, #build_info{
                        state=built,
                        fd=Fd,
                        pkg_name=Name,
                        description="ok"
                    }});
        {error, Value, Msg} ->
            ok = gen_server:call(caterpillar, 
                {err_built, self(), ToBuild, #build_info{
                        state=Value,
                        fd=none,
                        pkg_name=none,
                        description=Msg
                    }});
        Other ->
            logging:info_msg("build pipe failed with reason: ~p~n", [Other]),
            ok = gen_server:call(caterpillar,
                {
                    err_built, 
                    self(), 
                    ToBuild, #build_info{
                        state=none,
                        fd=none,
                        pkg_name=none,
                        description="unknown error"
                    }
                })
    end.

%% {{{1 Pipe functions

unpack_rev(Rev, {BuildPath, Buckets, DepsDets}) ->
    Package = ?VERSION(Rev),
    error_logger:info_msg("unpacking revision ~p~n", [Package]),
    Deps = ?CPU:get_dep_list(Rev#rev_def.pkg_config),
    case find_bucket(Buckets, Package, Deps) of
        [Bucket|_] ->
            error_logger:info_msg("found a bucket for ~p: ~p~n", [Rev, Bucket]),
            update_package_buckets(Buckets, DepsDets, Bucket, BuildPath, Rev),
            arm_build_bucket(Buckets, DepsDets, Bucket, BuildPath, Deps),
            {ok, {none, {Rev, Bucket, BuildPath}}};
        [] ->
            error_logger:info_msg("no bucket for ~p~n", [Package]),
            {ok, Bucket} = create_bucket(
                Buckets, DepsDets, Rev, BuildPath),
            arm_build_bucket(Buckets, DepsDets, Bucket, BuildPath, Deps),
            {ok, {none, {Rev, Bucket, BuildPath}}};
        _Other ->
            error_logger:error_msg("failed to find or create a bucket for ~p~n", [Rev]),
            {error, "failed to find a place to build, sorry"}
    end.
    

platform_get_env({Rev, {_, BPath, _}, BuildPath}, Plugins) ->
    {Name, _B, _T} = ?VERSION(Rev),
    PkgConfig = Rev#rev_def.pkg_config,
    Platform = PkgConfig#pkg_config.platform,
    Plugin = ?GV(Platform, Plugins, caterpillar_default_builder),
    Path = filename:join([BuildPath, BPath, binary_to_list(Name)]),
    {ok, Plugin, Path}.

package_get_env({Rev, {_, BPath, _}, BuildPath}, Plugins) ->
    {Name, _B, _T} = ?VERSION(Rev),
    PkgConfig = Rev#rev_def.pkg_config,
    PackageT = PkgConfig#pkg_config.package_t,
    Plugin = ?GV(PackageT, Plugins, caterpillar_deb_plugin),
    Path = filename:join([BuildPath, BPath, binary_to_list(Name)]),
    {ok, Plugin, Path}.

platform_clean(Env, Plugins) ->
    {ok, Plugin, Path} = platform_get_env(Env, Plugins),
    informer(none, Plugin:clean(Path), Env).

platform_test(Env, Plugins) ->
    {ok, Plugin, Path} = platform_get_env(Env, Plugins),
    informer(tested, Plugin:test(Path), Env).


platform_prebuild(Env, Plugins) ->
    {ok, Plugin, Path} = platform_get_env(Env, Plugins),
    informer(tested, Plugin:prebuild(Path), Env).

build_prepare(Env, Plugins) ->
    {ok, Plugin, Path} = package_get_env(Env, Plugins),
    informer(tested, Plugin:prepare(Path), Env).

build_check(Env, Plugins) ->
    {ok, Plugin, Path} = package_get_env(Env, Plugins),
    informer(tested, Plugin:check(Path), Env).

build_submit(Env, Plugins) ->
    {ok, Plugin, Path} = package_get_env(Env, Plugins),
    {State, Msg} = Plugin:submit(Path),
    case State of
        ok ->
            {_Fd, _Name} = Msg,
            Msg;
        error ->
            {error, Msg};
        Other ->
            error_logger:error_msg("failed to submit package: ~p~n", [Other]),
            {error, "unknown error while submitting package"}
    end.

informer(Phase, {State, Msg}, Env) ->
    case State of
        ok ->
            {ok, {Phase, Env}};
        error ->
            {error, Msg}
    end.


%% 1}}}

-spec create_bucket(reference(), reference(), version(), list()) ->
    {ok, BucketRef :: term()} | {error, Reason :: term()}.
% @doc Creates a directory pool for building some packages
create_bucket(BucketsDets, DepsDets, Package, BuildPath) ->
    BucketId = get_new_bucket(BucketsDets),
    Bucket = {list_to_binary(BucketId), BucketId, []},
    update_package_buckets(BucketsDets, DepsDets, Bucket, BuildPath, Package),
    {ok, Bucket}.


-spec find_bucket(reference(), version(), list()) ->
     [BucketRef :: term()] | [] | {error, term()}.
% @doc Find a bucket suitable for current version of the package.
find_bucket(BucketDets, Package, Deps) ->
    dets:traverse(BucketDets, 
        fun({_Id, _Path, Entries} = Bucket) ->
            case validate_bucket(Entries, [Package|Deps]) of
                true ->
                    {done, Bucket};
                false ->
                    continue
            end;
        (_Other) ->
            continue
        end).

validate_bucket(Entries, Deps) ->
    validate_bucket(Entries, Deps, true).
validate_bucket([], _Deps, Prev) ->
    Prev;
validate_bucket([E|O], Deps, Prev) ->
    {Name, Branch, Tag} = E,
    Res = case [{B, T} || {N, B, T} <- Deps, N == Name] of
        [{Branch, Tag}|_] ->
            true;
        [] ->
            true;
        _Other ->
            false
    end,
    validate_bucket(O, Deps, Res and Prev).

get_new_bucket(Dets) ->
    case dets:lookup(Dets, last_bucket) of
        [{last_bucket, Num}|_] ->
            ok = dets:insert(Dets, {last_bucket, Num+1}),
            io_lib:format("~4..0B", [Num + 1]);
        [] ->
            ok = dets:insert(Dets, {last_bucket, 0}),
            io_lib:format("~4..0B", [0])
    end.


arm_build_bucket(_Buckets, _Deps, _Current, _BuildPath, []) ->
    {ok, done};
arm_build_bucket(BucketsDets, Deps, Current, BuildPath, [Dep|O]) ->
    {BName, BPath, BPackages} = Current,
    case lists:member(Dep, BPackages) of
        true ->
            pass;
        false ->
            {Name, _B, _T} = Dep,
            [{Dep, {built, DepBuckets}, DepOn, HasInDep}|_] = dets:lookup(Deps, Dep),
            [AnyBucket|_] = DepBuckets,
            [{AnyBucket, Path, _Packages}] = dets:lookup(BucketsDets, AnyBucket),
            DepPath = filename:join([BuildPath, Path, binary_to_list(Name)]),
            ?CU:recursive_copy(DepPath, filename:join([BuildPath, BPath, binary_to_list(Name)])),
            dets:insert(BucketsDets, {BName, BPath, [BPackages|BPath]}),
            dets:insert(Deps, {Dep, {built, [DepBuckets|Current]}, DepOn, HasInDep})
    end,
    arm_build_bucket(BucketsDets, Deps, Current, BuildPath, O).

update_package_buckets(BucketsTable, DepsTable, Bucket, BuildPath, Rev) ->
    error_logger:info_msg("updating package bucket: ~p~n", [Bucket]),
    Package = ?VERSION(Rev),
    error_logger:info_msg("looking for package in Dets: ~p~n", [dets:lookup(DepsTable, Package)]),
    [{Package, {State, DepBucketIds}, DepOn, HasInDep}|_] = dets:lookup(DepsTable, Package),
    DepBuckets = lists:map(
        fun(Id) -> 
            [Res|_] = dets:lookup(BucketsTable, Id), 
            Res 
        end, DepBucketIds),
    AllBuckets = lists:usort([Bucket|DepBuckets]),
    {ok, SuccessBuckets} = update_buckets(BucketsTable, BuildPath, Rev, AllBuckets, []),
    dets:insert(DepsTable, 
        {Package, {State, SuccessBuckets}, DepOn, HasInDep}),
    ?CU:del_dir(get_temp_path(BuildPath, Rev)).

update_buckets(_, _, _, [], Acc) ->
    {ok, Acc};
update_buckets(BucketsTable, BuildPath, Rev, [Bucket|O], Acc) ->
    Package = ?VERSION(Rev),
    {BName, BPath, BContain} = Bucket,
    {Name, _B, _T} = Package,
    Path = filename:join([BuildPath, BPath, binary_to_list(Name)]) ++ "/",
    ?CU:del_dir(Path),
    filelib:ensure_dir(Path),
    error_logger:info_msg("trying to copy ~s to ~s~n", [get_temp_path(BuildPath, Rev), Path]),
    ok = ?CU:recursive_copy(get_temp_path(BuildPath, Rev), Path),
    ok = dets:insert(BucketsTable, {BName, BPath, lists:usort([Package|BContain])}),
    update_buckets(BucketsTable, BuildPath, Rev, O, [BName|Acc]).

get_temp_path(BuildPath, Rev) ->
    filename:join([BuildPath, "temp", ?CPU:get_dir_name(Rev)]).
