-module(caterpillar_router_tests).

-on_load(tty_off/0).

-include_lib("eunit/include/eunit.hrl").


tty_off() ->
    error_logger:tty(false).


tty_on() ->
    error_logger:tty(true).



start_link_test_() ->
{setup,
    fun() -> ok end,
    fun(_) -> ok = caterpillar_router:stop() end,
    fun() ->
        Res = caterpillar_router:start_link([]),
        ?assertMatch({ok, _}, Res),
        {ok, Pid} = Res,
        ?assertEqual(Pid, global:whereis_name(caterpillar_router))
    end
}.


stop_test_() ->
{setup,
    fun() -> caterpillar_router:start_link([]) end,
    fun(_) -> ok end,
    fun() ->
        Pid = global:whereis_name(caterpillar_router),
        ?assert(is_pid(Pid)),
        ?assert(is_process_alive(Pid)),
        ok = caterpillar_router:stop(),
        ?assert(not is_process_alive(Pid))
    end
}.
