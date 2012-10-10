-module(caterpillar_pkg_utils_tests).
-include_lib("eunit/include/eunit.hrl").
-include("caterpillar.hrl").
-define(CPU, caterpillar_pkg_utils).

-on_load(tty_off/0).


tty_off() -> 
    error_logger:tty(false).

-ifdef(TEST).

parse_control_test() ->
    Sample = 
    [
        {"Package", "caterpillar"},
        {"Section", "smprc"},
        {"Description", "\"package building tools\""},
        {"Architecture", "all"},
        {"Version", "0.0.1"},
        {"Maintainer", "vavdiushkin@smprc.ru"},
        {"Depends", ["erlang-base"]}
    ],
    {control, Res} = ?CPU:parse_control("./"),
    lists:map(
        fun({P, V}) ->
            ?assertEqual(?GV(P, Res), V)
        end, Sample).

-endif.
