-module(caterpillar_default_builder).

-export([prepare/2, clean/2, test/2, prebuild/2]).

-include("caterpillar_builder_internal.hrl").
-define(CMD, caterpillar_utils:command).
-define(LTB, list_to_binary).

prepare(_Rev, _Dir) ->
    {ok, <<>>}.

clean(Rev, Dir) ->
    error_logger:info_msg("executing make clean in ~s:~n", [Dir]),
    case ?CMD(get_command(Rev#rev_def.branch, "clean"), [{cwd, Dir}]) of
        {0, Msg} ->
            {ok, ?LTB("\nclean:\n" ++ Msg)};
        {110, Msg} ->
            error_logger:info_msg("clean timeout: ~s", [Msg]),
            {error, ?LTB("\nclean:\n" ++ Msg)};
        {Code, Msg} when is_integer(Code) ->
            error_logger:info_msg("clean failed with status ~B: ~s", [Code, Msg]),
            {ok, ?LTB("\nclean:\n" ++ Msg)}
    end.

test(Rev, Dir) ->
    error_logger:info_msg("executing make test in ~s:~n", [Dir]),
    case ?CMD(get_command(Rev#rev_def.branch, "test"), [{cwd, Dir}]) of
        {0, Msg} ->
            {ok, ?LTB("\ntest:\n" ++ Msg)};
        {Code, Msg} ->
            error_logger:info_msg("test failed with status ~B: ~s", [Code, Msg]),
            {error, ?LTB("\ntest:\n" ++ Msg)}
    end.


prebuild(_Rev, _Dir) ->
    {ok, <<>>}.


get_command(Branch, Type) ->
    lists:flatten(
        io_lib:format(
            "make ~s BRANCH=~s PATH_MOD=../*/ PATH_MK=../devel-tools/Makefile.mk",
            [Type, binary_to_list(Branch)]
        )
    ).
