-module(caterpillar_git_plugin).

-behaviour(caterpillar_repository_plugin).
-include_lib("caterpillar_repository_internal.hrl").

-export([init_plugin/1, terminate_plugin/1]).
-export([get_diff/5, get_changelog/5, get_revno/3]).
-export([is_repository/2, is_branch/3]).
-export([get_branches/2]).
-export([export/5]).
-export([get_tag/4]).
-export([init_repository/2]).



init_plugin(_Args) ->
    {ok, []}.


terminate_plugin(_State) ->
    ok.



export(_State, Package, Branch, _Revno, ExportPath) ->
    ExportBranch = format(
        "GIT_DIR=~s git archive heads/~s |tar -x -C ~s",
        [Package, Branch, ExportPath]
    ),
    case os:cmd(ExportBranch) of
        [] -> ok;
        Error -> {error, Error}
    end.



get_revno(_State, Package, Branch) ->
    GetRevno = format("GIT_DIR=~s git rev-parse heads/~s", [Package, Branch]),
    case os:cmd(GetRevno) of
        "fatal:" ++ Error ->
            {error, Error};
        "heads" ++ _ = Error ->
            {error, Error};
        Revno ->
            {ok, string:strip(Revno, right, $\n)}
    end.


is_repository(_State, Package) ->
    IsRepo = format("GIT_DIR=~s git rev-parse", [Package]),
    case os:cmd(IsRepo) of
        [] -> true;
        _ -> 
            error_logger:info_msg("~s is not repository~n", [Package]),
            false
    end.


is_branch(_State, Package, Branch) ->
    IsBranch = format("GIT_DIR=~s git rev-parse heads/~s 1> /dev/null", [Package, Branch]),
    case os:cmd(IsBranch) of
        [] -> true;
        Error ->
            error_logger:info_msg("not a branch: ~s~n", [Error]),
            false
    end.


get_branches(_State, Package) ->
    GetBranches = format("GIT_DIR=~s git branch", [Package]),
    case os:cmd(GetBranches) of
        "fatal:" ++ Error ->
            {error, Error};
        Result ->
            {ok, [string:strip(Branch, both, $ ) || Branch <- string:tokens(Result, "\n")]}
    end.


get_diff(_State, Package, _Branch, Revno, NewRevno) when Revno == none; Revno == error ->
    Revisions = os:cmd(format("GIT_DIR=~s git rev-list --all", [Package])),
    First = lists:last(string:tokens(Revisions, "\n")),
    DiffCmd = format("GIT_DIR=~s git diff ~s ~s", [Package, First, NewRevno]),
    case os:cmd(DiffCmd) of
        "error" ++ _ = Error -> {error, Error};
        Result ->
            Len = length(Result),
            if
                Len > 10240 -> {ok, list_to_binary(string:sub_string(Result, 1, 10240))};
                true -> {ok, list_to_binary(Result)}
            end
    end;


get_diff(_State, Package, _Branch, OldRevno, NewRevno) ->
    DiffCmd = format("GIT_DIR=~s git diff ~s ~s", [Package, OldRevno, NewRevno]),
    case os:cmd(DiffCmd) of 
        "error" ++ _ = Error -> {error, Error};
        Diff ->
            Len = length(Diff),
            if
                Len > 10240 -> {ok, list_to_binary(string:sub_string(Diff, 1, 10240))};
                true -> {ok, list_to_binary(Diff)}
            end
    end.


get_changelog(_State, Package, _Branch, Revno, NewRevno) when Revno == none; Revno == error ->
    LogCmd = format("GIT_DIR=~s git log ~s", [Package, NewRevno]),
    case os:cmd(LogCmd) of
        Log when Log > 10240 -> {ok, list_to_binary(string:sub_string(Log, 1, 10240))};
        Log -> {ok, list_to_binary(Log)}
    end;

get_changelog(_State, Package, _Branch, OldRevno, NewRevno) ->
    LogCmd = format("GIT_DIR=~s git log ~s..~s", [Package, OldRevno, NewRevno]),
    case os:cmd(LogCmd) of
        Log when Log > 10240 -> {ok, list_to_binary(string:sub_string(Log, 1, 10240))};
        Log -> {ok, list_to_binary(Log)}
    end.


get_tag(_State, Package, _Branch, NewRevno) ->
    TagCmd = format("GIT_DIR=~s git describe --exact-match ~s 2> /dev/null", [Package, NewRevno]),
    case os:cmd(TagCmd) of
        [] -> {ok, none};
        Tag -> {ok, string:strip(Tag, right, $\n)}
    end.


init_repository(#state{repository_root=RR}, Package) ->
    AbsPackage = filename:join(RR, Package),
    InitRepo = format("GIT_DIR=~s git init --shared --bare", [AbsPackage]),
    {ok, os:cmd(InitRepo)}. 


format(Command, Args) ->
    erlang:garbage_collect(),
    lists:flatten(io_lib:format(Command, Args)).
