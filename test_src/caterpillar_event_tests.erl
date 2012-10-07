-module(caterpillar_event_tests).

-on_load(tty_off/0).

-include_lib("eunit/include/eunit.hrl").


tty_off() ->
    error_logger:tty(false).


tty_on() ->
    error_logger:tty(true).



start_link_test_() ->
{setup,
    fun() -> ok end,
    fun(_) -> catch caterpillar_event:stop() end,
    fun() ->
        Res = caterpillar_event:start_link([]),
        ?assertMatch({ok, _}, Res),
        {ok, Pid} = Res,
        ?assertEqual(Pid, global:whereis_name(caterpillar_event))
    end
}.


stop_test_() ->
{setup,
    fun() -> caterpillar_event:start_link([]) end,
    fun(_) -> ok end,
    fun() ->
        Pid = global:whereis_name(caterpillar_event),
        ?assert(is_pid(Pid)),
        ?assert(is_process_alive(Pid)),
        caterpillar_event:stop(),
        timer:sleep(1),
        ?assert(not is_process_alive(Pid))
    end
}.
