-module(caterpillar_app).

-behaviour(application).

-export([start/0, stop_node/1, status_node/1]).
-export([start/2, stop/1]).


start() ->
    application:start(caterpillar).


stop_node([Node]) ->
    case net_adm:ping(Node) of
        pong ->
            rpc:call(Node, init, stop, []),
            io:format("stopped~n");
        pang ->
            io:format("down or unknown~n")
    end.


status_node([Node]) ->
    case net_adm:ping(Node) of
        pong ->
            Pid = rpc:call(Node, os, getpid, [], 10000),
            io:format("up at pid ~s~n", [Pid]),
            case global:whereis_name(caterpillar_event) of
                undefined -> ok;
                Pid -> 
                    io:format(
                        "registered services ~p~n", 
                        [gen_server:call(Pid, get_info, infinity)]
                    )
            end;
        pang ->
            io:format("Node ~p is down or bad cookie ~p~n",
            [Node, erlang:get_cookie()]
        )
    end.



start(_StartType, _StartArgs) ->
    caterpillar_sup:start_link().


stop(_State) ->
    ok.
