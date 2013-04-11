-module(caterpillar_deb_plugin).

-export([check/2, prepare/2, submit/2]).

-define(CMD, caterpillar_utils:command).
-include("caterpillar_internal.hrl").

check(_Rev, _Dir) ->
    {ok, ""}.

prepare(Rev, Dir) ->
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

submit(_Rev, Dir) ->
    error_logger:info_msg("executing make package in ~s:~n", [Dir]),
    case ?CMD("make package PATH_MOD=../*/ PATH_MK=../devel-tools/Makefile.mk PATH_PY_MK=../devel-tools/Makefile-py.mk", Dir) of
        {0, _Msg} ->
            find_deb_file(filename:join([Dir, "dist"]));
        {Code, Msg} when is_integer(Code) ->
            error_logger:info_msg("make package failed with status ~B: ~s", [Code, Msg]),
            {error, io_lib:format("make package returned ~B: ~s", [Code, Msg])}
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
