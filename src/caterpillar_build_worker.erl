-module(caterpillar_build_worker).

-include_lib("caterpillar.hrl").
-include_lib("caterpillar_internal.hrl").

-behaviour(gen_server).

-define(DEFAULT_BUILD_PATH, "/srv/caterpillar").
-define(DEFAULT_BUCKETS_DETS, "/var/lib/smprc/caterpillar/buckets").
-define(DEFAULT_DEPENDENCIES_DETS, "/var/lib/smprc/caterpillar/deps").
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

-export([get_new_bucket/1]). %% for test compile now

start_link() ->
    gen_server:start_link(?MODULE, [], []).

start_link(Settings) ->
    gen_server:start_link(?MODULE, [Settings], []).

init(Settings) ->
    BuildPlugins = ?GV(
        build_plugins, 
        Settings, 
        [{deb, caterpillar_deb_plugin}]
    ),
    PlatformPlugins = ?GV(
        platform_plugins, 
        Settings, 
        [{default, caterpillar_default_builder}]
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
    case catch caterpillar_utils:build_pipe(Funs, ToBuild) of
        {ok, Info} ->
            ok = gen_server:call(caterpillar, 
                {built, self(), ToBuild, Info});
        {error, Info} ->
            ok = gen_server:call(caterpillar, 
                {err_built, self(), ToBuild, Info});
        _Other ->
            Info = unknown_error,
            ok = gen_server:call(caterpillar,
                {
                    err_built, 
                    self(), 
                    ToBuild, #build_info{}
                })
    end,
    {ok, Info}.

%% {{{1 Pipe functions

unpack_rev(Rev, {BuildPath, Buckets, DepsDets}) ->
    Package = ?VERSION(Rev),
    Deps = ?CPU:get_dep_list(Rev#rev_def.pkg_config),
    case find_bucket(Buckets, Package, Deps) of
        [Bucket|_] ->
            put_package_to_bucket(Buckets, DepsDets, Bucket, BuildPath, Rev),
            arm_build_bucket(Buckets, DepsDets, Package, Deps);
        [] ->
            create_bucket(Buckets, DepsDets, Package, BuildPath),
            arm_build_bucket(Buckets, DepsDets, Package, Deps);
        _Other ->
            error_logger:error_msg("failed to find or create a bucket for ~p~n", [Rev]),
            {error, create_bucket_failed}
    end.

platform_clean(_PkgInfo, _Plugins) ->
    {ok, none}.

platform_test(_PkgInfo, _Plugins) ->
    {ok, none}.

platform_prebuild(_PkgInfo, _Plugins) ->
    {ok, none}.

build_prepare(_PkgInfo, _Plugins) ->
    {ok, none}.

build_check(_PkgInfo, _Plugins) ->
    {ok, none}.

build_submit(_PkgInfo, _Plugins) ->
    {ok, none}.

%% 1}}}

-spec create_bucket(reference(), reference(), version(), list()) ->
    {ok, BucketRef :: binary()} | {error, Reason :: term()}.
% @doc Creates a directory pool for building some packages
create_bucket(BucketsDets, DepsDets, Package, BuildPath) ->
    Bucket = get_new_bucket(BucketsDets),
    put_package_to_bucket(BucketsDets, DepsDets, Bucket, BuildPath, Package).


-spec find_bucket(reference(), version(), list()) ->
     [BucketRef :: binary()] | [] | {error, term()}.
% @doc Find a bucket suitable for current version of the package.
find_bucket(BucketDets, Package, Deps) ->
    dets:traverse(BucketDets, 
        fun({_Id, _Path, Entries} = Bucket) ->
            case validate_bucket(Entries, [Package|Deps]) of
                true ->
                    {done, Bucket};
                false ->
                    continue
            end
        end).

validate_bucket(Entries, Deps) ->
    validate_bucket(Entries, Deps, true).
validate_bucket([], _Deps, Prev) ->
    Prev;
validate_bucket([E|O], Deps, Prev) ->
    {Name, Bucket, Tag} = E,
    Res = case [{B, T} || {N, B, T} <- Deps, N == Name] of
        [{Bucket, Tag}|_] ->
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
            ok = dets:insert(Dets, 0),
            io_lib:format("~4..0B", [0])
    end.


arm_build_bucket(_Buckets, _Deps, _Current, []) ->
    {ok, done};
arm_build_bucket(BucketsDets, Deps, Current, [Dep|O]) ->
    {BName, BPath, BPackages} = Current,
    {Name, _B, _T} = Dep,
    [{Dep, {built, DepBuckets}, DepOn, HasInDep}|_] = dets:lookup(Deps, Dep),
    [AnyBucket|_] = DepBuckets,
    [{AnyBucket, Path, _Packages}] = dets:lookup(BucketsDets, AnyBucket),
    DepPath = Path ++ "/" ++ Name,
    case filelib:is_dir(DepPath) of
        true ->
            file:rename(DepPath, BPath ++ "/" ++ Name),
            dets:insert(BucketsDets, {BName, BPath, [BPackages|BPath]}),
            dets:insert(Deps, {Dep, {built, [DepBuckets|Current]}, DepOn, HasInDep}),
            arm_build_bucket(BucketsDets, Deps, Current, [Dep|O]);
        false ->
            {error, no_dir}
    end.

put_package_to_bucket(Buckets, Deps, Bucket, BuildPath, Rev) ->
    Package = ?VERSION(Rev),
    [{Package, {built, DepBuckets}, DepOn, HasInDep}|_] = dets:lookup(Deps, Package),
    {Name, _B, _T} = Package,
    Path = BuildPath ++ "/" ++ Bucket ++ "/" ++ binary_to_list(Name),
    ?CU:del_dir(Path),
    filelib:ensure_dir(Path),
    TempPath = BuildPath ++ "/temp/" ++ ?CPU:get_dir_name(Rev),
    case filelib:is_dir(TempPath) of
        true ->
            ok = file:rename(TempPath, Path),
            ok = dets:insert(Buckets, {Bucket, Path, [Package]}),
            ok = dets:insert(Deps, {Package, {built, [DepBuckets|Bucket]}, DepOn, HasInDep}),
            {ok, list_to_binary(Bucket)};
        false ->
            {error, no_unpacked_package}
    end.
