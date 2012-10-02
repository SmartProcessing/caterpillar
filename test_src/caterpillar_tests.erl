-module(caterpillar_tests).
-include_lib("eunit/include/eunit.hrl").
-include("caterpillar.hrl").

-on_load(tty_off/0).


tty_off() ->
    error_logger:tty(false).


-ifdef(TEST).

setup() ->
    application:start(loggins),
    application:start(caterpillar),
    dets:open_file('test', [{file, "test.dets"}]).

cleanup(_Ign) ->
    dets:close('test'),
    os:cmd("rm test.dets"),
    application:stop(caterpillar).

caterpillar_init_test() ->
    {ok, [Settings]} = file:consult("test.config"),
    CaterpillarSettings = proplists:get_value(caterpillar, Settings),
    _HandlerSettings = proplists:get_value(handler, CaterpillarSettings).

-endif.
