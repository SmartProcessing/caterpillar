-module(caterpillar_build_worker).

-include_lib("caterpillar.hrl").
-include_lib("caterpillar_builder_internal.hrl").

-behaviour(gen_server).

-define(CU, caterpillar_utils).
-define(CPU, caterpillar_pkg_utils).
-define(CBS, caterpillar_build_storage).
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
            ok = gen_server:call(caterpillar_builder, 
                {built, self(), ToBuild, #build_info{
                        state = <<"built">>,
                        fd=Fd,
                        pkg_name=Name,
                        description="ok"
                    }}, infinity),
            make_complete_actions(<<"built">>, Env, DepsDets, BuildBuckets);
        {error, Value, Msg, Env} ->
            ok = gen_server:call(caterpillar_builder, 
                {err_built, self(), ToBuild, #build_info{
                        state=Value,
                        fd=none,
                        pkg_name=none,
                        description=Msg
                    }}, infinity),
            make_complete_actions(Value, Env, DepsDets, BuildBuckets);
        {error, Value, Msg} ->
            ok = gen_server:call(caterpillar_builder, 
                {err_built, self(), ToBuild, #build_info{
                        state=Value,
                        fd=none,
                        pkg_name=none,
                        description=Msg
                    }}, infinity);
        Other ->
            error_logger:info_msg("build pipe failed with reason: ~p~n", [Other]),
            ok = gen_server:call(caterpillar_builder,
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
    Res = case ?CBS:find_bucket(Buckets, Rev) of
        [Bucket] ->
            create_workspace(Buckets, DepsDets, Bucket, BuildPath, Rev),
            {ok, 
                {none, {Rev, Bucket, BuildPath}}
            };
        [] ->
            {ok, Bucket} = ?CBS:create_bucket(Buckets, Rev),
            create_workspace(Buckets, DepsDets, Bucket, BuildPath, Rev),
            {ok, 
                {none, {Rev, Bucket, BuildPath}}
            };
        _Other ->
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



make_complete_actions(
    Status, 
    {Rev, {BName, BPath, _}, BuildPath}, 
    DepsDets, 
    BucketsDets) when Status == <<"built">>; Status == <<"tested">>
->
    InBuckets = ?CBS:list_buckets(DepsDets, Rev),
    UpdateInBuckets = lists:delete(BName, InBuckets),
    Buckets = lists:map(fun(X) -> 
                ?CBS:fetch_bucket(BucketsDets, X) end, UpdateInBuckets),
    Source = filename:join([BuildPath, BPath, binary_to_list(Rev#rev_def.name)]),
    ?CBS:update_buckets(BucketsDets, BuildPath, Source, Rev, Buckets, []);
make_complete_actions(
    _Status, 
    {Rev, {BName, BPath, _}, Path}, 
    DepsDets, 
    BucketsDets) 
->
    lists:map(fun(X) -> 
                ?CBS:delete_from_bucket(BucketsDets, Path, X, Rev)
            end, ?CBS:list_buckets(DepsDets, Rev)),
    ?CBS:empty_state_buckets(DepsDets, Rev).

create_workspace(Buckets, DepsDets, Bucket, BuildPath, Rev) ->
    try
        Deps = [{N, B, T} || {{N, B, T}, _S} <- Rev#rev_def.dep_object],
        {ok, [NewBucket]} = ?CBS:update_dep_buckets(
            Buckets,
            DepsDets, 
            [Bucket], 
            BuildPath, 
            ?CBS:get_temp_path(BuildPath, Rev),
            Rev),
        ?CBS:arm_bucket(Buckets, DepsDets, NewBucket, BuildPath, Deps)
    catch
        _:Reason ->
            error_logger:error_msg("failed to unpack revision ~p to bucket ~p: ~p~n", 
                [Rev, Bucket, erlang:get_stacktrace()]),
            throw(Reason)
    end.
