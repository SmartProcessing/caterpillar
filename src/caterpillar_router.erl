-module(caterpillar_router).

-behaviour(gen_server).

-export([start_link/1, stop/0]).
-export([init/1, handle_info/2, handle_cast/2, handle_call/3, code_change/3, terminate/2]).


start_link(_Args) -> gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).


stop() ->
    gen_server:call({global, ?MODULE}, stop, infinity).


init(_) ->
    {ok, []}.


handle_info(_Msg, State) ->
    {noreply, State}.



handle_cast(_Msg, State) ->
    {noreply, State}.



handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Msg, _From, State) ->
    {reply, {error, bad_msg}, State}.


code_change(_Old, State, _Extra) ->
    {ok, State}.


terminate(Reason, State) ->
    error_logger:info_msg("caterpillar_router down with reason: ~p~n", [Reason]).
