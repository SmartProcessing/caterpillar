-module(caterpillar_default_builder).

-export([clean/1, test/1, prebuild/1]).

-define(CMD, caterpillar_utils:command).

clean(Dir) ->
    case ?CMD("make clean", Dir) of
        {0, Msg} ->
            {ok, Msg};
        {110, Msg} ->
            error_logger:info_msg("clean timeout: ~s", [Msg]),
            {error, Msg};
        {Code, Msg} when is_integer(Code) ->
            error_logger:info_msg("clean failed with status ~B: ~s", [Code, Msg]),
            {ok, Msg}
    end.

test(Dir) ->
    case ?CMD("make test PATH_MOD=../ PATH_MK=../devel-tools/Makefile.mk", Dir) of
        {0, Msg} ->
            {ok, Msg};
        {Code, Msg} when is_integer(Code) ->
            error_logger:info_msg("clean failed with status ~B: ~s", [Code, Msg]),
            {error, io_lib:format("make test returned ~B: ~s", [Code, Msg])}
    end.

prebuild(Dir) ->
    {ok, ""}.


