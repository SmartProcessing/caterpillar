-module(caterpillar_build_worker).

-include_lib("caterpillar.hrl").
-include_lib("caterpillar_internal.hrl").

-behaviour(gen_server).

-define(CU, caterpillar_utils).
-define(CPU, caterpillar_pkg_utils).
-define(LOCK(X), gen_server:call(caterpillar_lock, {lock, X}, infinity)).
-define(UNLOCK(X), gen_server:call(caterpillar_lock, {unlock, X})).


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
    error_logger:info_msg("received build request: ~p~n", [?VERSION(ToBuild)]),
    PlatformPlugins = State#state.platform_plugins,
    BuildPlugins = State#state.build_plugins,
    BuildPath = State#state.build_path,
    BuildBuckets = State#state.buckets,
    DepsDets = State#state.deps,
    Funs = [
        {fun unpack_rev/2, {BuildPath, BuildBuckets, DepsDets}},
        {fun platform_clean/2, PlatformPlugins},
        {fun platform_test/2, PlatformPlugins},
        {fun platform_prebuild/2, PlatformPlugins},
        {fun build_prepare/2, BuildPlugins},
        {fun build_check/2, BuildPlugins},
        {fun build_submit/2, BuildPlugins}
    ],
    case catch caterpillar_utils:build_pipe(Funs, {none, ToBuild}) of
        {ok, {{Fd, Name}, Env}} ->
            ok = gen_server:call(caterpillar, 
                {built, self(), ToBuild, #build_info{
                        state = <<"built">>,
                        fd=Fd,
                        pkg_name=Name,
                        description="ok"
                    }}, infinity),
            make_complete_actions(<<"built">>, Env, DepsDets, BuildBuckets);
        {error, Value, Msg, Env} ->
            ok = gen_server:call(caterpillar, 
                {err_built, self(), ToBuild, #build_info{
                        state=Value,
                        fd=none,
                        pkg_name=none,
                        description=Msg
                    }}, infinity),
            make_complete_actions(Value, Env, DepsDets, BuildBuckets);
        {error, Value, Msg} ->
            ok = gen_server:call(caterpillar, 
                {err_built, self(), ToBuild, #build_info{
                        state=Value,
                        fd=none,
                        pkg_name=none,
                        description=Msg
                    }}, infinity);
        Other ->
            error_logger:info_msg("build pipe failed with reason: ~p~n", [Other]),
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
                }, infinity)
    end.

%% {{{{2 Pipe functions

unpack_rev(Rev, {BuildPath, Buckets, DepsDets}) ->
    Package = ?VERSION(Rev),
    Deps = [{N, B, T} || {{N, B, T}, _} <- Rev#rev_def.dep_object],
    error_logger:info_msg("unpacking revision ~p~n", [Package]),
    Res = case find_bucket(Buckets, Package, Deps) of
        [Bucket] ->
            error_logger:info_msg("found a bucket for ~p: ~p~n", [Package, Bucket]),
            create_workspace(Buckets, DepsDets, Bucket, BuildPath, Rev),
            {ok, 
                {none, {Rev, Bucket, BuildPath}}
            };
        [] ->
            error_logger:info_msg("no bucket for ~p, creating new~n", [Package]),
            {ok, Bucket} = create_bucket(Buckets, Rev),
            create_workspace(Buckets, DepsDets, Bucket, BuildPath, Rev),
            {ok, 
                {none, {Rev, Bucket, BuildPath}}
            };
        _Other ->
            error_logger:error_msg("failed to find or create a bucket for ~p~n", [Package]),
            {error, "failed to find a place to build, sorry"}
    end,
    Res.
    

platform_get_env({Rev, {_, BPath, _}, BuildPath}, Plugins) ->
    {Name, _B, _T} = ?VERSION(Rev),
    PkgConfig = Rev#rev_def.pkg_config,
    Platform = PkgConfig#pkg_config.platform,
    Plugin = ?GV(Platform, Plugins, caterpillar_default_builder),
    Path = filename:join([BuildPath, BPath, binary_to_list(Name)]),
    {ok, Plugin, Path, Rev}.

package_get_env({Rev, {_, BPath, _}, BuildPath}, Plugins) ->
    {Name, _B, _T} = ?VERSION(Rev),
    PkgConfig = Rev#rev_def.pkg_config,
    PackageT = PkgConfig#pkg_config.package_t,
    Plugin = ?GV(PackageT, Plugins, caterpillar_deb_plugin),
    Path = filename:join([BuildPath, BPath, binary_to_list(Name)]),
    {ok, Plugin, Path, Rev}.

platform_clean(Env, Plugins) ->
    {ok, Plugin, Path, Rev} = platform_get_env(Env, Plugins),
    informer(<<"none">>, Plugin:clean(Rev, Path), Env).

platform_test(Env, Plugins) ->
    {ok, Plugin, Path, Rev} = platform_get_env(Env, Plugins),
    informer(<<"tested">>, Plugin:test(Rev, Path), Env).


platform_prebuild(Env, Plugins) ->
    {ok, Plugin, Path, Rev} = platform_get_env(Env, Plugins),
    informer(<<"tested">>, Plugin:prebuild(Rev, Path), Env).

build_prepare(Env, Plugins) ->
    {ok, Plugin, Path, Rev} = package_get_env(Env, Plugins),
    informer(<<"tested">>, Plugin:prepare(Rev, Path), Env).

build_check(Env, Plugins) ->
    {ok, Plugin, Path, Rev} = package_get_env(Env, Plugins),
    informer(<<"tested">>, Plugin:check(Rev, Path), Env).

build_submit(Env, Plugins) ->
    {ok, Plugin, Path, Rev} = package_get_env(Env, Plugins),
    {State, Msg} = Plugin:submit(Rev, Path),
    case State of
        ok ->
            {_Fd, _Name} = Msg,
            {ok, {Msg, Env}};
        error ->
            {error, Msg, Env};
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


%% 2}}}}

-spec create_bucket(reference(), #rev_def{}) ->
    {ok, BucketRef :: term()} | {error, Reason :: term()}.
% @doc Creates a directory pool for building some packages
create_bucket(BucketsDets, Rev) ->
    Package = ?VERSION(Rev),
    BucketId = get_new_bucket(BucketsDets),
    Bucket = {list_to_binary(BucketId), BucketId, [Package]},
    {ok, Bucket}.


-spec find_bucket(reference(), version(), list()) ->
     [BucketRef :: term()] | [] | {error, term()}.
% @doc Find a bucket suitable for current version of the package.
find_bucket(BucketDets, Package, Deps) ->
    iter_dets(BucketDets, Package, Deps).

iter_dets(BucketDets, Package, Deps) ->
    First = dets:first(BucketDets),
    iter_dets(BucketDets, Package, Deps, First).

iter_dets(_BucketDets, _Package, _Deps, '$end_of_table') ->
    [];
iter_dets(BucketDets, Package, Deps, BucketId) ->
    ?LOCK(BucketId),
    B = dets:lookup(BucketDets, BucketId),
    case B of
        [{Id, Path, Entries}] = [Bucket] ->
            case validate_bucket(Entries, [Package|Deps]) of
                true ->
                    dets:insert(BucketDets, {Id, Path, lists:usort([Package|Entries] ++ Deps)}),
                    ?UNLOCK(BucketId),
                    [Bucket];
                false ->
                    ?UNLOCK(BucketId),
                    iter_dets(BucketDets, Package, Deps, dets:next(BucketDets, BucketId))
            end;
        _Other ->
            ?UNLOCK(BucketId),
            iter_dets(BucketDets, Package, Deps, dets:next(BucketDets, BucketId))
    end.

validate_bucket(Entries, Deps) ->
    validate_bucket(Entries, Deps, true).
validate_bucket([], _Deps, Prev) ->
    Prev;
validate_bucket([E|O], Deps, Prev) -> 
    {Name, Branch, Tag} = E,
    Res = case [{B, T} || {N, B, T} <- Deps, N == Name] of
        [{B, T}|_] when B /= Branch; T /= Tag ->
            false;
        [{B, T}|_] when B == Branch, T == Tag ->
            true;
        [] ->
            true;
        _Other ->
            false
    end,
    validate_bucket(O, Deps, Res and Prev).

get_new_bucket(Dets) ->
    ?LOCK(last_bucket),
    LastBucket = dets:lookup(Dets, last_bucket),
    Res = case LastBucket of
        [{last_bucket, Num}|_] ->
            ok = dets:insert(Dets, {last_bucket, Num+1}),
            io_lib:format("~4..0B", [Num + 1]);
        [] ->
            ok = dets:insert(Dets, {last_bucket, 0}),
            io_lib:format("~4..0B", [0])
    end,
    ?UNLOCK(last_bucket),
    Res.


arm_build_bucket(_Buckets, _Deps, _Current, _BuildPath, []) ->
    {ok, done};
arm_build_bucket(BucketsDets, Deps, Current, BuildPath, [Dep|O]=Dependencies) ->
    {BName, _, _} = Current,
    {Name, _, _} = Dep,
    ?LOCK(BName),
    [{BName, BPath, BPackages}] = dets:lookup(BucketsDets, BName),
    ?UNLOCK(BName),
    ?LOCK(Dep),
    [{Dep, {_, DepBuckets}, _, _}|_] = dets:lookup(Deps, Dep),
    ?UNLOCK(Dep),
    case lists:member(BName, DepBuckets) of
        true ->
            pass;
        false ->
            case [X || X <- DepBuckets, X /= BName] of
                [AnyBucket|_] ->
                    ?LOCK(AnyBucket),
                    [{AnyBucket, Path, _Packages}] = dets:lookup(BucketsDets, AnyBucket),
                    ?UNLOCK(AnyBucket),
                    DepPath = filename:join([BuildPath, Path, binary_to_list(Name)]),
                    copy_package_to_bucket(DepPath, filename:join([BuildPath, BPath, binary_to_list(Name)])),
                    ?LOCK(Dep),
                    [{Dep, {NewState, NewDepBuckets}, NewDepOn, NewHasInDep}|_] = dets:lookup(Deps, Dep),
                    ok = dets:insert(Deps, {Dep, {NewState, lists:usort([BName|NewDepBuckets])}, NewDepOn, NewHasInDep}),
                    ok = dets:insert(BucketsDets, {BName, BPath, [Dep|BPackages]}),
                    ?UNLOCK(Dep);
                [] ->
                    DepPath = get_temp_path(BuildPath, Dep),
                    copy_package_to_bucket(DepPath, filename:join([BuildPath, BPath, binary_to_list(Name)])),
                    ?LOCK(Dep),
                    [{Dep, {NewState, NewDepBuckets}, NewDepOn, NewHasInDep}|_] = dets:lookup(Deps, Dep),
                    ok = dets:insert(Deps, {Dep, {NewState, lists:usort([BName|NewDepBuckets])}, NewDepOn, NewHasInDep}),
                    ok = dets:insert(BucketsDets, {BName, BPath, [Dep|BPackages]}),
                    ?UNLOCK(Dep),
                    ?LOCK(BName),
                    [{BName, BPath, OldContain}] = dets:lookup(BucketsDets, BName),
                    ok = dets:insert(BucketsDets, {BName, BPath, [Dep|OldContain]}),
                    ?UNLOCK(BName)
            end
    end,
    arm_build_bucket(BucketsDets, Deps, {BName, BPath, [Dep|BPackages]}, BuildPath, O).

update_package_buckets(BucketsTable, DepsTable, Buckets, BuildPath, Source, Rev) ->
    Package = ?VERSION(Rev),
    case catch update_buckets(BucketsTable, BuildPath, Source, Rev, Buckets, []) of
        {ok, UpdatedBuckets} ->
            SuccessB = [X || {X, _, _} <- UpdatedBuckets],
            ?LOCK(Package),
            [{Package, {NewState, NewBucketList}, NewDepObj, NewDepSubj}|_] = dets:lookup(DepsTable, Package),
            ok = dets:insert(DepsTable, 
                {Package, {NewState, lists:usort(SuccessB ++ NewBucketList)}, NewDepObj, NewDepSubj}),
            ?UNLOCK(Package),
            {ok, UpdatedBuckets};
        Other ->
            error_logger:error_msg("failed to update package buckets for ~p: ~p", [Package, Other]),
            % ?UNLOCK(Package),
            throw(Other)
    end.

update_buckets(_, _, _, _, [], Acc) ->
    {ok, Acc};
update_buckets(BucketsTable, BuildPath, Source, Rev, [Bucket|O], Acc) ->
    Package = ?VERSION(Rev),
    {BName, TempPath, TempContain} = Bucket,
    Deps = [{N, B, T} || {{N, B, T}, _S} <-Rev#rev_def.dep_object],
    ?LOCK(BName),
    [{BName, BPath, BContain}] = case dets:lookup(BucketsTable, BName) of
        [] -> 
            [{BName, TempPath, TempContain}];
        [{B, P, C}] ->
            [{B, P, C}]
    end,
    {Name, _B, _T} = Package,
    Path = filename:join([BuildPath, BPath, binary_to_list(Name)]) ++ "/",
    NewContain = lists:usort([Package|BContain] ++ Deps),
    try
        copy_package_to_bucket(Source, Path),
        ok = dets:insert(BucketsTable, {BName, BPath, lists:usort(NewContain)}),
        ?UNLOCK(BName)
    catch
        _:Reason ->
            ?UNLOCK(BName),
            error_logger:error_msg("failed to update bucket ~p: ~p", [BName, Reason]),
            throw(Reason)
    end,
    update_buckets(BucketsTable, BuildPath, Source, Rev, O, [{BName, BPath, NewContain}|Acc]).


copy_package_to_bucket(Source, Path) ->
    error_logger:info_msg("copying ~p to ~p", [Source, Path]),
    ?CU:del_dir(Path),
    filelib:ensure_dir(Path),
    ?CU:recursive_copy(Source, Path).

get_temp_path(BuildPath, Rev) ->
    filename:join([BuildPath, "temp", ?CPU:get_dir_name(Rev)]).

make_complete_actions(
    Status, 
    {Rev, {BName, BPath, _}, BuildPath}, 
    DepsDets, 
    BucketsDets) when Status == <<"built">>; Status == <<"tested">>
->
    Version = ?VERSION(Rev),
    ?LOCK(Version),
    [{Version, {_, InBuckets}, _, _}] = dets:lookup(DepsDets, Version),
    ?UNLOCK(Version),
    UpdateInBuckets = lists:delete(BName, InBuckets),
    Buckets = lists:map(fun(X) -> 
                ?LOCK(X),
                [Res] = dets:lookup(BucketsDets, X), 
                ?UNLOCK(X),
                Res end, UpdateInBuckets),
    Source = filename:join([BuildPath, BPath, binary_to_list(Rev#rev_def.name)]),
    update_buckets(BucketsDets, BuildPath, Source, Rev, Buckets, []);
make_complete_actions(
    _Status, 
    {Rev, {BName, BPath, _}, BuildPath}, 
    DepsDets, 
    BucketsDets) 
->
    Version = ?VERSION(Rev),
    ?LOCK(Version),
    [{Version, {_, InBuckets}, _, _}] = dets:lookup(DepsDets, Version),
    ?UNLOCK(Version),
    lists:map(fun(X) -> 
                ?LOCK(X),
                [{BName, BPath, BContain}] = dets:lookup(BucketsDets, X), 
                dets:insert(BucketsDets, {BName, BPath, lists:delete(Version, BContain)}),
                ?UNLOCK(X),
                ToClean = filename:join([BuildPath, BPath, binary_to_list(Rev#rev_def.name)])
                % error_logger:info_msg("was cleaning: ~p~n", [ToClean])
                % ?CU:del_dir(ToClean)
            end, InBuckets).

create_workspace(Buckets, DepsDets, Bucket, BuildPath, Rev) ->
    try
        Deps = [{N, B, T} || {{N, B, T}, _S} <- Rev#rev_def.dep_object],
        {ok, [NewBucket]} = update_package_buckets(
            Buckets,
            DepsDets, 
            [Bucket], 
            BuildPath, 
            get_temp_path(BuildPath, Rev),
            Rev),
        % ?CU:del_dir(get_temp_path(BuildPath, Rev)),
        arm_build_bucket(Buckets, DepsDets, NewBucket, BuildPath, Deps)
    catch
        _:Reason ->
            error_logger:error_msg("failed to unpack revision ~p to bucket ~p: ~p~n", 
                [Rev, Bucket, erlang:get_stacktrace()]),
            throw(Reason)
    end.
