-module(caterpillar_default_builder).

-export([clean/1, test/1, prebuild/1]).

-define(CMD, caterpillar_utils:command).

clean(Dir) ->
    error_logger:info_msg("executing make clean in ~s:~n", [Dir]),
    case ?CMD("make clean PATH_MOD=../*/ PATH_MK=../devel-tools/Makefile.mk PATH_PY_MK=../devel-tools/Makefile-py.mk", Dir) of
        {0, _Msg} ->
            {ok, ""};
        {110, Msg} ->
            error_logger:info_msg("clean timeout: ~s", [Msg]),
            {error, Msg};
        {Code, Msg} when is_integer(Code) ->
            error_logger:info_msg("clean failed with status ~B: ~s", [Code, Msg]),
            {ok, ""}
    end.

test(Dir) ->
    error_logger:info_msg("executing make test in ~s:~n", [Dir]),
    case ?CMD("make test PATH_MOD=../*/ PATH_MK=../devel-tools/Makefile.mk PATH_PY_MK=../smprc.setup/Makefile.mk", Dir) of
        {0, _Msg} ->
            {ok, ""};
        {Code, Msg} when is_integer(Code) ->
            error_logger:info_msg("test failed with status ~B: ~s", [Code, Msg]),
            {error, io_lib:format("errors on test~n ~B: ~s", [Code, Msg])}
    end.

prebuild(_Dir) ->
    {ok, ""}.
