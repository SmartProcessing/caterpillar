-module(caterpillar_default_builder).

-export([clean/2, test/2, prebuild/2]).

-include("caterpillar_internal.hrl").
-define(CMD, caterpillar_utils:command).


clean(Rev, Dir) ->
    error_logger:info_msg("executing make clean in ~s:~n", [Dir]),
    case ?CMD(get_command(Rev#rev_def.branch, "clean"), [{cwd, Dir}]) of
        {0, _Msg} ->
            {ok, ""};
        {110, Msg} ->
            error_logger:info_msg("clean timeout: ~s", [Msg]),
            {error, Msg};
        {Code, Msg} when is_integer(Code) ->
            error_logger:info_msg("clean failed with status ~B: ~s", [Code, Msg]),
            {ok, ""}
    end.


test(Rev, Dir) ->
    error_logger:info_msg("executing make test in ~s:~n", [Dir]),
    case ?CMD(get_command(Rev#rev_def.branch, "test"), [{cwd, Dir}]) of
        {0, _Msg} ->
            {ok, ""};
        {Code, Msg} when is_integer(Code) ->
            error_logger:info_msg("test failed with status ~B: ~s", [Code, Msg]),
            {error, io_lib:format("errors on test~n ~B: ~s", [Code, Msg])}
    end.


prebuild(_Rev, _Dir) ->
    {ok, ""}.


get_command(Branch, Type) ->
    lists:flatten(
        io_lib:format(
            "make ~s BRANCH=~s PATH_MOD=../*/ PATH_MK=../devel-tools/Makefile.mk PATH_PY_MK=../devel-tools/Makefile-py.mk",
            [Type, binary_to_list(Branch)]
        )
    ).
