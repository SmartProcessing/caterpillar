-module(caterpillar_git_local).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-export([start/1]).
-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start(Settings) ->
    gen_server:start_link(?MODULE, Settings, []).

start_link(Settings) ->
    gen_server:start_link(?MODULE, [Settings], []).

init(Settings) ->
    logging:info_msg("starting caterpillar_git_local plugin~n", []),
    {ok, {state, Settings}}.


handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
