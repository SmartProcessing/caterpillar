-module(caterpillar_git_plugin).

-behaviour(caterpillar_repository_plugin).
-include_lib("caterpillar_repository_internal.hrl").

-export([init_plugin/1, terminate_plugin/1]).
-export([get_diff/5, get_changelog/5, get_revno/3]).
-export([is_repository/2, is_branch/3]).
-export([get_branches/2]).
-export([export_archive/5]).
-export([get_tag/4]).
-export([init_repository/2]).



init_plugin(_Args) ->
    {ok, []}.


terminate_plugin(_State) ->
    ok.



export_archive(_State, Package, Branch, _Revno, ExportPath) ->
    ExportBranch = format("git archive heads/~s ~s", [Branch, ExportPath]),
    case command(ExportBranch, [{cd, Package}]) of
        {ok, <<>>} -> ok;
        Error -> {error, Error}
    end.



get_revno(_State, Package, Branch) ->
    GetRevno = format("git rev-parse heads/~s", [Branch]),
    case command(GetRevno, [{cd, Package}]) of
        {error, Error} -> {error, Error};
        {ok, Revno} -> {ok, binary:replace(Revno, <<"\n">>, <<>>)}
    end.


is_repository(_State, Package) ->
    IsRepo = "git rev-parse",
    case command(IsRepo, [{cd, Package}]) of
        {ok, <<>>} -> true;
        {error, _} -> 
            error_logger:info_msg("~s is not repository~n", [Package]),
            false
    end.


is_branch(_State, Package, Branch) ->
    IsBranch = format("git rev-parse heads/~s 1> /dev/null", [Branch]),
    case command(IsBranch, [{cd, Package}]) of
        {ok, <<>>} -> true;
        {error, Error} ->
            error_logger:info_msg("not a branch: ~s~n", [Error]),
            false
    end.


get_branches(_State, Package) ->
    GetBranches = "git rev-parse --abbrev-ref --branches",
    case command(GetBranches, [{cd, Package}]) of
        {error, _}=Error -> Error;
        {ok, <<>>} -> {ok, []};
        {ok, Result} -> 
            Branches = [
                string:strip(binary_to_list(Branch), both, $ ) ||
                Branch <- binary:split(Result, <<"\n">>, [global]), Branch /= <<>>
            ],
            {ok, Branches}
    end.


get_diff(_State, Package, _Branch, Revno, NewRevno) when Revno == none; Revno == error ->
    Revisions = case command("git rev-list --all", [{cd, Package}]) of
        {ok, Data} -> Data;
        {error, Err} ->  error_logger:error_msg("get_diff error: ~p~n", [Err]), <<>>
    end,
    First = lists:last([Rev || Rev <- binary:split(Revisions, <<"\n">>, [global]), Rev /= <<>>]),
    DiffCmd = format("git diff ~s ~s", [First, NewRevno]),
    case command(DiffCmd, [{cd, Package}]) of
        {error, _} = Error -> Error; 
        {ok, Diff} ->
            Size = size(Diff),
            if
                Size > 10240 -> {ok, binary:part(Diff, 0, 10239)};
                true -> {ok, Diff}
            end
    end;


get_diff(_State, Package, _Branch, OldRevno, NewRevno) ->
    DiffCmd = format("git diff ~s ~s", [OldRevno, NewRevno]),
    case command(DiffCmd, [{cd, Package}]) of 
        {error, _} = Error -> Error;
        {ok, Diff} ->
            Size = size(Diff),
            if
                Size > 10240 -> {ok, binary:part(Diff, 0, 10239)};
                true -> {ok, Diff}
            end
    end.


get_changelog(_State, Package, _Branch, Revno, NewRevno) when Revno == none; Revno == error ->
    LogCmd = format("git log ~s", [NewRevno]),
    case command(LogCmd, [{cd, Package}]) of
        {error, _} = Error -> Error;
        {ok, Log} ->
            Size = size(Log),
            if
                Size > 10240 -> {ok, binary:part(Log, 0, 10239)};
                true -> {ok, Log}
            end
    end;

get_changelog(_State, Package, _Branch, OldRevno, NewRevno) ->
    LogCmd = format("git log ~s..~s", [OldRevno, NewRevno]),
    case command(LogCmd, [{cd, Package}]) of
        {error, _} = Error -> Error;
        {ok, Log} ->
            Size = size(Log),
            if
                Size > 10240 -> {ok, binary:part(Log, 0, 10239)};
                true -> {ok, Log}
            end
    end.


get_tag(_State, Package, _Branch, NewRevno) ->
    TagCmd = format("git describe --exact-match ~s 2> /dev/null", [NewRevno]),
    case command(TagCmd, [{cd, Package}]) of
        {ok, <<>>} -> {ok, ""};
        {ok, Tag} -> {ok, binary_to_list(binary:replace(Tag, <<"\n">>, <<>>, [global]))};
        {error, _} -> {ok, ""}
    end.



%----------- custom commands


init_repository(#state{repository_root=RR, vcs_state=VCState}, RawPackage) ->
    Package = case is_binary(RawPackage) of
        true -> binary_to_list(RawPackage);
        false -> RawPackage
    end,
    AbsPackage = filename:join(RR, Package),
    case is_repository(VCState, AbsPackage) of
        true -> {error, <<"already initialied">>};
        false ->
            InitRepo = "git init --shared --bare",
            caterpillar_utils:ensure_dir(AbsPackage),
            command(InitRepo, [{cd, AbsPackage}])
    end.


%------------ auxiliary functions


format(Command, Args) ->
    erlang:garbage_collect(),
    lists:flatten(io_lib:format(Command, Args)).



command(Command, AdditionalOpts) ->
    Opts = [binary, use_stdio, exit_status, stderr_to_stdout] ++ AdditionalOpts,
    Port = erlang:open_port({spawn, Command}, Opts),
    Data = (catch recv()),
    catch erlang:port_close(Port),
    Data.



recv() ->
    recv(<<>>).


recv(Result) ->
    receive
        {_, {data, Data}} -> recv(<<Result/binary, Data/binary>>);
        {_, {exit_status, 0}} -> {ok, Result};
        {_, {exit_status, _}} -> {error, Result};
        Error -> exit(Error)
    end.
