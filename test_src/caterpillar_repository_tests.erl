-module(caterpillar_repository_tests).

-on_load(tty_off/0).

-include_lib("eunit/include/eunit.hrl").
-include_lib("caterpillar_repository_internal.hrl").

-define(ARGS, [{vcs_plugin, test_vcs_plugin}]).


tty_off() ->
    error_logger:tty(false).


tty_on() ->
    error_logger:tty(true).



start_link_test_() ->
{setup,
    fun() -> ok end,
    fun(_) -> ok = caterpillar_repository:stop() end,
    fun() ->
        Res = caterpillar_repository:start_link(?ARGS),
        ?assertMatch({ok, _}, Res),
        {ok, Pid} = Res,
        ?assertEqual(Pid, whereis(caterpillar_repository))
    end
}.



stop_test_() ->
{setup,
    fun() -> caterpillar_repository:start_link(?ARGS) end,
    fun(_) -> catch erlang:exit(whereis(caterpillar_repository), kill) end,
    fun() ->
        Pid = whereis(caterpillar_repository),
        ?assert(is_pid(Pid)),
        ?assert(is_process_alive(Pid)),
        ?assertEqual(ok, caterpillar_repository:stop()),
        ?assert(not is_process_alive(Pid))
    end
}.


vcs_init_test_() ->
{foreach,
    fun() -> ok end,
[
    {Message, fun() ->
        ?assertEqual(
            Result,
            catch caterpillar_repository:vcs_init(#state{}, Args)
        )
    end} || {Message, Args, Result} <- [
        {
            "vcs inited",
            [{vcs_plugin, test_vcs_plugin}],
            #state{vcs_plugin=test_vcs_plugin, vcs_state=state}
        },
        {
            "bad vcs callback, init failed",
            [{vcs_plugin, undefined}],
            {'EXIT', {vcs_init, failed}}
        }
    ]
]}.


scan_repository_test_() ->
{foreach,
    fun() -> ok end,
[
    {Message, fun() ->
        Check(caterpillar_repository:scan_repository(Data()))        
    end} || {Message, Check, Data} <- [
        {
            "scan_repository received integer as parameter",
            fun(Ref) ->
                ?assert(is_reference(Ref)),
                ?assert(erlang:cancel_timer(Ref) > 0)
            end,
            fun() -> 1000 end
        },
        {
            "scan_repository received integer as parameter, timer fires",
            fun(Ref) ->
                timer:sleep(1),
                ?assertEqual(
                    receive Any -> Any after 10 -> timeout end,
                    scan_repository
                )
            end,
            fun() -> 0 end
        },
        {
            "scan_repository received state as parameter with not cancelled timer, timer fires",
            fun(Ref) ->
                ?assertEqual(
                    receive Any -> Any after 10 -> timeout end,
                    scan_repository
                )
            end,
            fun() -> #state{
                    scan_timer=erlang:send_after(10, self(), ':D'),
                    scan_interval=1
                }
            end
        }
    ]
]}.
