-module(caterpillar_worker).
-include("caterpillar.hrl").
-behaviour(gen_server).
-define(DEFAULT_BUILD_PATH, "/srv/caterpillar").

-record(state, {
    build_plugins       :: [{atom(), list()}],
    platform_plugins    :: [{atom(), list()}],
    build_path          :: list()
}).

-export([start_link/1, start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

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
    {ok, #state{
        build_plugins=BuildPlugins,
        platform_plugins=PlatformPlugins,
        build_path=BuildPath
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
    Funs = [
        {fun unpack_rev/2, BuildPath},
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
            gen_server:call(caterpillar,
                {
                    err_built, 
                    self(), 
                    ToBuild, #build_info{}
                })
    end,
    {ok, Info}.


unpack_rev(_,_) ->
    {ok, none}.

get_pkg_info(_BuildPath, _Revision) ->
    {ok, none}.

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
