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
        [
            {"deb", caterpillar_deb_plugin},
            {"script", caterpillar_script_builder}
        ]
    ),
    PlatformPlugins = ?GV(
        platform_plugins, 
        Settings, 
        [
            {"default", caterpillar_default_builder},
            {"script", caterpillar_script_builder}
        ]
    ),
    BuildPath = ?GV(
        build_path, 
        Settings, 
        ?DEFAULT_BUILD_PATH 
    ),
    {ok, Deps} = dets:open_file(deps,
        [{file, ?GV(deps, Settings, ?DEFAULT_DEPENDENCIES_DETS)}]),
    {ok, #state{
        build_plugins=BuildPlugins,
        platform_plugins=PlatformPlugins,
        build_path=BuildPath,
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
    DepsDets = State#state.deps,
    Funs = [
        {fun unpack_rev/2, {BuildPath, DepsDets}},
        {fun platform_prepare/2, PlatformPlugins},
        {fun platform_clean/2, PlatformPlugins},
        {fun platform_test/2, PlatformPlugins},
        {fun platform_prebuild/2, PlatformPlugins},
        {fun build_prepare/2, BuildPlugins},
        {fun build_check/2, BuildPlugins},
        {fun build_submit/2, BuildPlugins}
    ],
    case catch caterpillar_utils:build_pipe(Funs, {none, ToBuild}) of
        {ok, {{Fd, Name}, BMsg, Env={_, _, OldMsg}}} ->
            informer(<<"built">>, {ok, <<>>}, Env),
            ok = gen_server:call(caterpillar_builder, 
                {built, self(), ToBuild, #build_info{
                        state = <<"built">>,
                        fd=Fd,
                        pkg_name=Name,
                        description= <<OldMsg/binary, BMsg/binary>>
                    }}, infinity),
            make_complete_actions(Env);
        {error, Value, Msg, Env={_, _, OldMsg}} ->
            ok = gen_server:call(caterpillar_builder, 
                {err_built, self(), ToBuild, #build_info{
                        state=Value,
                        fd=none,
                        pkg_name=none,
                        description= <<OldMsg/binary, Msg/binary>>
                    }}, infinity),
            make_complete_actions(Env);
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

unpack_rev(Rev, {BuildPath, DepsDets}) ->
    {ok, Msg} = create_workspace(DepsDets, BuildPath, Rev),
    {ok, 
        {none, {Rev, BuildPath, Msg}}
    }.
    
platform_get_env({Rev, BuildPath, Msg}, Plugins) ->
    {Name, _B, _T} = ?VERSION(Rev),
    PkgConfig = Rev#rev_def.pkg_config,
    Platform = PkgConfig#pkg_config.platform,
    Plugin = ?GV(Platform, Plugins, caterpillar_default_builder),
    Path = filename:join([?CBS:get_statpack_path(BuildPath, Rev, Rev#rev_def.work_id), binary_to_list(Name)]),
    {ok, Plugin, Path, Rev}.

package_get_env({Rev, BuildPath, Msg}, Plugins) ->
    {Name, _B, _T} = ?VERSION(Rev),
    PkgConfig = Rev#rev_def.pkg_config,
    [PackageT|_] = PkgConfig#pkg_config.package_t, 
    Plugin = ?GV(PackageT, Plugins, caterpillar_deb_plugin),
    Path = filename:join([?CBS:get_statpack_path(BuildPath, Rev, Rev#rev_def.work_id), binary_to_list(Name)]),
    {ok, Plugin, Path, Rev}.

platform_prepare(Env, Plugins) ->
    {ok, Plugin, Path, Rev} = platform_get_env(Env, Plugins),
    informer(<<"none">>, Plugin:prepare(Rev, Path), Env).

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
    informer(<<"tested">>, Plugin:build_prepare(Rev, Path), Env).

build_check(Env, Plugins) ->
    {ok, Plugin, Path, Rev} = package_get_env(Env, Plugins),
    informer(<<"tested">>, Plugin:build_check(Rev, Path), Env).

build_submit(Env, Plugins) ->
    {ok, Plugin, Path, Rev} = package_get_env(Env, Plugins),
    case Plugin:build_submit(Rev, Path) of
        {{ok, Fcontain}, Msg} ->
            {ok, {Fcontain, Msg, Env}};
        {{error, Msg}, Bmsg} ->
            {error, <<Bmsg/binary, Msg/binary>>, Env};
        Other ->
            error_logger:error_msg("failed to submit package: ~p~n", [Other]),
            {error, <<"unknown error while submitting package">>}
    end.

informer(Phase, S = {State, Msg}, Env={Rev, Path, EnvMsg}) ->
    case State of
        ok ->
            ?CBS:store_package_with_state(Phase, Rev, Path),
            {ok, {Phase, {Rev, Path, <<EnvMsg/binary, Msg/binary>>}}};
        error ->
            {error, <<EnvMsg/binary, Msg/binary>>}
    end.


%% 2}}}}

make_complete_actions({Rev, BuildPath, _}) ->
    ?CBS:delete_statpack(Rev, BuildPath).

create_workspace(DepsDets, BuildPath, Rev) ->
    try
        ?CBS:arm_bucket(Rev, DepsDets, BuildPath, Rev#rev_def.dep_object, <<"getting dependencies:\n">>)
    catch
        _:Reason ->
            error_logger:error_msg("failed to unpack revision ~p: ~p~n", 
                [Rev, erlang:get_stacktrace()]),
            throw(Reason)
    end.
