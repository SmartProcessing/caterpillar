-module(caterpillar_test_support).
-export([wait_for_exit/1]).
-export([recv/0, recv/1]).
-export([mock/2, kill_mock/1]).

-include_lib("eunit/include/eunit.hrl").

-spec wait_for_exit(PidOrName::pid()|atom()) -> ok.
wait_for_exit(Name) when is_atom(Name) ->
    case whereis(Name) of
        undefined -> ok;
        Pid -> wait_for_exit(Pid)
    end;
wait_for_exit(Pid) when is_pid(Pid) ->
    MRef = erlang:monitor(process, Pid),
    receive {'DOWN', MRef, _, _, _} -> ok end.


-spec recv() -> any()|timeout.
recv() -> recv(500).


recv(Timeout) -> receive A -> A after Timeout -> timeout end.


-spec mock(Name::atom(), Data::list()|tuple()) -> pid().
mock(Name, Data) when not is_list(Data) -> mock(Name, [Data]);
mock(Name, Data) when is_atom(Name) ->
    Callee = self(),
    Pid = spawn(fun() ->
        Self = self(),
        yes = global:register_name(Name, Self),
        true = register(Name, Self),
        Self ! {spawned, Self},
        loop(Data)
    end),
    receive
        {spawned, Pid} -> Pid
    end.


loop([{Request, Response}|Rest]) -> 
    receive 
        {_, From, Request} -> gen_server:reply(From, Response), loop(Rest);
        {_, From, BadRequest} -> gen_server:reply(From, {error, {{bad_request, BadRequest}, {expected, Request}}}), ok
    end;
loop([]) -> ok.


-spec kill_mock(atom()|pid()) -> ok.
kill_mock(Name) when is_atom(Name) ->
    case whereis(Name) of
        undefined -> ok;
        Pid -> kill_mock(Pid)
    end;
kill_mock(Pid) when is_pid(Pid) ->
    erlang:exit(Pid, kill),
    wait_for_exit(Pid),
    ok.
