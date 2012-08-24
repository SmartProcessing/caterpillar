-module(caterpillar_tests).
-include_lib("eunit/include/eunit.hrl").
-include("caterpillar.hrl").

-ifdef(EUNIT).

setup() ->
    application:start(loggins),
    application:start(caterpillar).

cleanup(_Ign) ->
    application:stop(caterpillar).

caterpillar_init_test() ->
    {ok, [Settings]} = file:consult("../test.config"),
    ?debugFmt("Settings=~p", [Settings]),
    CaterpillarSettings = proplists:get_value(caterpillar, Settings),
    HandlerSettings = proplists:get_value(handler, CaterpillarSettings),
    ?debugFmt("Init settings: ~p~n", [caterpillar:init(HandlerSettings)]).

-endif.
