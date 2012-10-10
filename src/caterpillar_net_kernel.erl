-module(caterpillar_net_kernel).

-behaviour(gen_server).

-export([init/1, handle_info/2, handle_cast/2, handle_call/3, code_change/3, terminate/2]).
-export([start_link/1, stop/0]).


start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).


stop() ->
    gen_server:call(?MODULE, stop, infinity).



init(Args) ->
    _ = os:cmd("epmd -daemon"),
    NodeName = proplists:get_value(self, Args),
    case net_kernel:start([NodeName, longnames]) of
        {ok, _} -> ok;
        Error ->
            error_logger:info_msg(
                "net_kernal failed to start with: ~p~n",
                [Error]
            ),
            init:stop()
    end,
    Cookie = proplists:get_value(cookie, Args, 'caterpillar'),
    true = erlang:set_cookie(node(), Cookie),
    {ok, []}.


handle_call(_, _, State) ->
    {reply, bad_msg, State}.


handle_info(_, State) ->
    {noreply, State}.


handle_cast(_, State) ->
    {noreply, State}.


terminate(Reason, _State) ->
    error_logger:info_msg("caterpillar net kernel down witn ~p~n", [Reason]).


code_change(_Old, State, _Extra) ->
    {ok, State}.

