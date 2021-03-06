-module(caterpillar_deb_plugin).

-export([build_check/2, build_prepare/2, build_submit/2]).

-define(CMD, caterpillar_utils:command).
-define(BTL, binary_to_list).
-define(LTB, list_to_binary).
-include("caterpillar_builder_internal.hrl").

build_check(_Rev, _Dir) ->
    {ok, <<>>}.

build_prepare(Rev, Dir) ->
    ControlFile = filename:join(Dir, "control"),
    PkgConfig = filename:join(Dir, "pkg.config"),
    case {filelib:is_regular(PkgConfig), filelib:is_regular(ControlFile)} of
        {false, true} ->
            Branch = binary_to_list(Rev#rev_def.branch),
            WorkId = Rev#rev_def.work_id,
            catch caterpillar_simple_builder:modify_control(ControlFile, Branch, WorkId, "all");
        _ ->
            file:write_file(filename:join(Dir, "control"), caterpillar_pkg_utils:gen_control_from_pkg_config(Rev))
    end,
    {ok, <<>>}.

build_submit(Rev, Dir) ->
    error_logger:info_msg("executing make package in ~s:~n", [Dir]),
    case ?CMD(get_command(
                [
                    {"BRANCH", ?BTL(Rev#rev_def.branch)},
                    {"BUILD_ID", integer_to_list(Rev#rev_def.work_id)}
                ], "package"), [{cwd, Dir}]) of
        {0, Msg} ->
            {find_deb_file(filename:join([Dir, "dist"])), ?LTB("\nsubmit:\n" ++ Msg)};
        {Code, Msg} when is_integer(Code) ->
            error_logger:info_msg("make package failed with status ~B: ~s", [Code, Msg]),
            {{error, ?LTB("\nsubmit:\n" ++ Msg)}, <<>>}
    end.

find_deb_file(Dir) ->
    case filelib:wildcard(filename:join(Dir, "*.deb")) of
        [Deb] ->
            Name = lists:last(filename:split(Deb)),
            {ok, Fd} = file:open(Deb, [read]),
            {ok, {Fd, Name}};
        Other ->
            error_logger:error_msg("cant find deb package, ~p~n", [Other]),
            {error, <<"couldn't find *.deb package">>}
    end.

get_command(Env, Type) ->
    lists:flatten(io_lib:format("make ~s ~s PATH_MOD=../*/ PATH_MK=../devel-tools/Makefile.mk",
        [Type, build_environment(Env)])).

build_environment(PL) ->
    Env = [[X, "=", Y] || {X, Y} <- PL],
    lists:flatten(string:join(Env, " ")).
