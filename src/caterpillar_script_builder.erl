-module(caterpillar_script_builder).

-export([prepare/2, clean/2, test/2, prebuild/2]).
-export([build_check/2, build_prepare/2, build_submit/2]).

-include("caterpillar_builder_internal.hrl").
-define(CMD, caterpillar_utils:command).

prepare(Rev, Dir) ->
    error_logger:info_msg("executing build.sh configure in ~s:~n", [Dir]),
    case ?CMD(get_command(Rev#rev_def.branch, "configure"), [{cwd, Dir}]) of
        {0, _Msg} ->
            {ok, ""};
        {Code, Msg} when is_integer(Code) ->
            error_logger:info_msg("configure step failed with status ~B: ~s", [Code, Msg]),
            {error, io_lib:format("errors on configure~n ~B: ~s", [Code, Msg])}
    end.

clean(Rev, Dir) ->
    error_logger:info_msg("executing build.sh clean in ~s:~n", [Dir]),
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
    error_logger:info_msg("executing build.sh test in ~s:~n", [Dir]),
    case ?CMD(get_command(Rev#rev_def.branch, "test"), [{cwd, Dir}]) of
        {0, _Msg} ->
            {ok, ""};
        {Code, Msg} when is_integer(Code) ->
            error_logger:info_msg("test failed with status ~B: ~s", [Code, Msg]),
            {error, io_lib:format("errors on test~n ~B: ~s", [Code, Msg])}
    end.

prebuild(_Rev, _Dir) ->
    {ok, ""}.

build_check(_Rev, _Dir) ->
    {ok, ""}.

build_prepare(Rev, Dir) ->
    ControlFile = filename:join(Dir, "control"),
    case filelib:is_regular(ControlFile) of
        true ->
            Branch = binary_to_list(Rev#rev_def.branch),
            WorkId = Rev#rev_def.work_id,
            catch caterpillar_simple_builder:modify_control(ControlFile, Branch, WorkId, "all");
        _ ->
            file:write_file(filename:join(Dir, "control"), caterpillar_pkg_utils:gen_control_from_pkg_config(Rev), [exclusive])
    end,
    {ok, ""}.

build_submit(Rev, Dir) ->
    error_logger:info_msg("executing build.sh package in ~s:~n", [Dir]),
    case ?CMD(get_command(Rev#rev_def.branch, "package"), [{cwd, Dir}]) of
        {0, _Msg} ->
            find_deb_file(filename:join([Dir, "dist"]));
        {Code, Msg} when is_integer(Code) ->
            error_logger:info_msg("make package failed with status ~B: ~s", [Code, Msg]),
            {error, lists:flatten(io_lib:format("make package returned ~B: ~s", [Code, Msg]))}
    end.

find_deb_file(Dir) ->
    case filelib:wildcard(filename:join(Dir, "*.deb")) of
        [Deb] ->
            Name = lists:last(filename:split(Deb)),
            {ok, Fd} = file:open(Deb, [read]),
            {ok, {Fd, Name}};
        Other ->
            error_logger:error_msg("cant find deb package, ~p~n", [Other]),
            {error, "couldn't find *.deb package"}
    end.



get_command(Branch, Type) ->
    lists:flatten(
        io_lib:format(
            "BRANCH=~s build.sh ~s",
            [binary_to_list(Branch), Type]
        )
    ).
