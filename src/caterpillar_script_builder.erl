-module(caterpillar_script_builder).

-export([prepare/2, clean/2, test/2, prebuild/2]).
-export([build_check/2, build_prepare/2, build_submit/2]).

-include("caterpillar_builder_internal.hrl").
-define(CMD, caterpillar_utils:command).
-define(LTB, list_to_binary).


prepare(#rev_def{branch=Branch}=Rev, Dir) ->
    error_logger:info_msg("executing build.sh configure in ~s:~n", [Dir]),
    case ?CMD(get_command("configure"), [{cwd, Dir}, {env, get_env(Branch)}]) of
        {0, Msg} ->
            {ok, ?LTB(Msg)};
        {Code, Msg} ->
            error_logger:info_msg("configure step failed with status ~B: ~s", [Code, Msg]),
            {error, ?LTB("\nconfigure:\n" ++ Msg)}
    end.


clean(#rev_def{branch=Branch}=Rev, Dir) ->
    error_logger:info_msg("executing build.sh clean in ~s:~n", [Dir]),
    case ?CMD(get_command("clean"), [{cwd, Dir}, {env, get_env(Branch)}]) of
        {0, Msg} ->
            {ok, ?LTB("\nclean:\n" ++ Msg)};
        {110, Msg} ->
            error_logger:info_msg("clean timeout: ~s", [Msg]),
            {error, ?LTB("\nclean:\n" ++ Msg)};
        {Code, Msg} ->
            error_logger:info_msg("clean failed with status ~B: ~s", [Code, Msg]),
            {ok, ?LTB("\nclean:\n" ++ Msg)}
    end.


test(#rev_def{branch=Branch}=Rev, Dir) ->
    error_logger:info_msg("executing build.sh test in ~s:~n", [Dir]),
    case ?CMD(get_command("test"), [{cwd, Dir}, {env, get_env(Branch)}]) of
        {0, Msg} ->
            {ok, ?LTB("\ntest:\n" ++ Msg)};
        {Code, Msg} ->
            error_logger:info_msg("test failed with status ~B: ~s", [Code, Msg]),
            {error, ?LTB("\ntest:\n" ++ Msg)}
    end.


prebuild(_Rev, _Dir) -> {ok, <<>>}.


build_check(_Rev, _Dir) -> {ok, <<>>}.


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


build_submit(#rev_def{branch=Branch}=Rev, Dir) ->
    error_logger:info_msg("executing build.sh package in ~s:~n", [Dir]),
    case ?CMD(get_command("package"), [{cwd, Dir}, {env, get_env(Branch)}]) of
        {0, Msg} ->
            {find_deb_file(filename:join([Dir, "dist"])), ?LTB("\npackage:\n" ++ Msg)};
        {Code, Msg} ->
            error_logger:info_msg("make package failed with status ~B: ~s", [Code, Msg]),
            {error, ?LTB("\nsubmit:\n" ++ Msg)}
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



get_command(Type) ->
    format("./build.sh ~s", [Type]).


get_env(Branch) ->
    [{"BRANCH", format("~s", [Branch])}].


format(Template, Args) -> lists:flatten(io_lib:format(Template, Args)).
