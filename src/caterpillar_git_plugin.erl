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
    case command(ExportBranch) of
        [] -> ok;
        Error -> {error, Error}
    end.



get_revno(_State, Package, Branch) ->
    GetRevno = format("GIT_DIR=~s git rev-parse heads/~s", [Package, Branch]),
    case command(GetRevno) of
        {error, Error} -> {error, Error};
        {ok, Revno} -> {ok, binary:replace(Revno, <<"\n">>, <<>>)}
    end.


is_repository(_State, Package) ->
    IsRepo = format("GIT_DIR=~s git rev-parse", [Package]),
    case command(IsRepo) of
        {ok, <<>>} -> true;
        {error, _} -> 
            error_logger:info_msg("~s is not repository~n", [Package]),
            false
    end.


is_branch(_State, Package, Branch) ->
    IsBranch = format("GIT_DIR=~s git rev-parse heads/~s 1> /dev/null", [Package, Branch]),
    case command(IsBranch) of
        {ok, <<>>} -> true;
        {error, Error} ->
            error_logger:info_msg("not a branch: ~s~n", [Error]),
            false
    end.


get_branches(_State, Package) ->
    GetBranches = format("GIT_DIR=~s git branch", [Package]),
    case command(GetBranches) of
        {error, _}=Error -> Error;
        {ok, <<>>} -> {ok, []};
        {ok, Result} -> 
            {ok, [Branch || Branch <- binary:split(Result, <<"\n">>, [global]), Branch /= <<>>]}
    end.


get_diff(_State, Package, _Branch, Revno, NewRevno) when Revno == none; Revno == error ->
    Revisions = command(format("GIT_DIR=~s git rev-list --all", [Package])),
    First = lists:last(string:tokens(Revisions, "\n")),
    DiffCmd = format("GIT_DIR=~s git diff ~s ~s", [Package, First, NewRevno]),
    case command(DiffCmd) of
        {error, _} = Error -> Error; 
        {ok, Diff} ->
            Size = size(Diff),
            if
                Size > 10240 -> {ok, binary:part(Diff, 0, 10239)};
                true -> {ok, Diff}
            end
    end;


get_diff(_State, Package, _Branch, OldRevno, NewRevno) ->
    DiffCmd = format("GIT_DIR=~s git diff ~s ~s", [Package, OldRevno, NewRevno]),
    case command(DiffCmd) of 
        {error, _} = Error -> Error;
        {ok, Diff} ->
            Size = size(Diff),
            if
                Size > 10240 -> {ok, binary:part(Diff, 0, 10239)};
                true -> {ok, Diff}
            end
    end.


get_changelog(_State, Package, _Branch, Revno, NewRevno) when Revno == none; Revno == error ->
    LogCmd = format("GIT_DIR=~s git log ~s", [Package, NewRevno]),
    case command(LogCmd) of
        {error, _} = Error -> Error;
        {ok, Log} ->
            Size = size(Log),
            if
                Size > 10240 -> {ok, binary:part(Log, 0, 10239)};
                true -> {ok, Log}
            end
    end;

get_changelog(_State, Package, _Branch, OldRevno, NewRevno) ->
    LogCmd = format("GIT_DIR=~s git log ~s..~s", [Package, OldRevno, NewRevno]),
    case command(LogCmd) of
        {error, _} = Error -> Error;
        {ok, Log} ->
            Size = size(Log),
            if
                Size > 10240 -> {ok, binary:part(Log, 0, 10239)};
                true -> {ok, Log}
            end
    end.


get_tag(_State, Package, _Branch, NewRevno) ->
    TagCmd = format("GIT_DIR=~s git describe --exact-match ~s 2> /dev/null", [Package, NewRevno]),
    case command(TagCmd) of
        [] -> {ok, none};
        Tag -> {ok, string:strip(Tag, right, $\n)}
    end.


init_repository(#state{repository_root=RR}, Package) ->
    AbsPackage = filename:join(RR, Package),
    InitRepo = format("GIT_DIR=~s git init --shared --bare", [AbsPackage]),
    {ok, command(InitRepo)}. 


format(Command, Args) ->
    erlang:garbage_collect(),
    lists:flatten(io_lib:format(Command, Args)).



command(Command) ->
    Opts = [binary, use_stdio, exit_status, stderr_to_stdout],
    Port = erlang:open_port({spawn, Command}, Opts),
    error_logger:info_msg("Command: ~n~p~n", [Command]),
    Data = (catch recv()),
    error_logger:info_msg("Data: ~n~p~n", [Data]),
    erlang:port_close(Port),
    Data.



recv() ->
    recv(<<>>).


recv(Result) ->
    receive
        {_, {data, Data}} -> recv(<<Result/binary, Data/binary>>);
        {_, {exit_status, 0}} -> {ok, Result};
        {_, {exit_status, N}} -> {error, Result}
    end.
