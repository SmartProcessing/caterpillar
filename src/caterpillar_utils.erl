-module(caterpillar_utils).

-include_lib("caterpillar.hrl").
-include_lib("caterpillar_internal.hrl").
-include_lib("kernel/include/file.hrl").

-export([get_version_by_revdef/1, build_pipe/2]).
-export([pipe/3]).
-export([read_work_id/1, write_work_id/2]).
-export([package_to_archive/2, archive_to_package/1]).
-export([list_packages/1, del_dir/1, recursive_copy/2]).
-export([ensure_dir/1]).
-export([get_value_or_die/2]).
-export([command/1, command/2, command/4]).

-define(DEFAULT_TIMEOUT, 300000).

-type function_spec()   :: {function(), [term()]}.

-spec read_work_id(Filename::file:filename()) -> BuildId::pos_integer().
-spec write_work_id(Filename::file:filename(), BuildId::pos_integer()) -> ok | {error, Reason::term()}.



-spec get_version_by_revdef(RevDef :: #rev_def{}) -> Version :: version().
get_version_by_revdef(RevDef) ->
    Name = RevDef#rev_def.name,
    Branch = RevDef#rev_def.branch,
    Tag = RevDef#rev_def.tag,
    {Name, Branch, Tag}.

-spec build_pipe([function_spec()], term()) -> {ok, term()} | {error, term()}.
build_pipe([], Value) ->
    {ok, Value};
build_pipe([Fun|O], {Phase, Value}) ->
    {Function, Opts} = Fun,
    case erlang:apply(Function, [Value, Opts]) of
        {ok, Res} ->
            build_pipe(O, Res);
        {error, Msg} ->
            error_logger:info_msg("build pipe failed:~p~n", [Msg]),
            {error, Phase, Msg};
        {error, Msg, Env} ->
            error_logger:info_msg("build pipe failed:~p~n", [Msg]),
            {error, Phase, Msg, Env};
        Other ->
            error_logger:info_msg("build pipe failed:~p~n", [Other]),
            {error, Phase, io_lib:format("~p", [Other])}
    end.


pipe([], PrevResult, _State) -> {ok, PrevResult};
pipe([{Name, Fun}|T], InitialResult, State) ->
    case catch Fun(InitialResult, State) of
        {ok, NewResult} -> pipe(T, NewResult, State);
        {ok, NewResult, NewState} -> pipe(T, NewResult, NewState);
        {stop, NewResult} -> {ok, NewResult};
        {error, Reason} -> {error, Reason};
        Error ->
            error_logger:error_msg("pipe unmatched result in ~p: ~p~n", [Name, Error]), 
            {error, {pipe, Error}}
    end.



read_work_id(Filename) ->
    check_work_id_file(Filename),
    case file:consult(Filename) of
        {ok, [{work_id, BuildId}]} when is_integer(BuildId) ->
            BuildId;
        _ ->
            write_work_id(Filename, 1),
            1
    end.


write_work_id(Filename, BuildId) when is_integer(BuildId)->
    file:write_file(
        Filename,
        io_lib:format("~p.", [{work_id, BuildId}])
    );
write_work_id(_, BuildId) -> 
    error_logger:error_msg("write_work_id: bad work_id - ~p~n", [BuildId]),
    {error, badarg}.


check_work_id_file(Filename) ->
    case filelib:is_regular(Filename) of
        true -> ok;
        false ->
            filelib:ensure_dir(Filename),
            file:write_file(Filename, <<>>)
    end.



package_to_archive(Repo, Branch) ->
    <<(to_binary(Repo))/binary, "__ARCHIVE__", (to_binary(Branch))/binary>>.


archive_to_package(Archive) ->
    [Repo, Branch] = string:tokens(to_list(Archive), "__ARCHIVE__"),
    {to_binary(Repo), to_binary(Branch)}.



list_packages(Path) ->
    case file:list_dir(Path) of
        {ok, List} -> list_packages(List, []);
        Error -> Error
    end.


list_packages([], Accum) ->
    {ok, lists:sort(Accum)};
list_packages([ [$.|_]|O ], Accum) ->
    list_packages(O, Accum);
list_packages([ H|O ], Accum) ->
    list_packages(O, [unicode:characters_to_binary(H)|Accum]).


del_dir(Dir) ->
    case filelib:is_dir(Dir) of
        true ->
            {ok, All} = file:list_dir(Dir),
            del_dir(Dir, All);
        false ->
            file:delete(Dir) 
    end.


del_dir(Dir, [H|T]) when H /= <<".">> andalso H /= <<"..">> ->
    AbsPath = filename:join(Dir, H),
    case filelib:is_dir(AbsPath) of
        false ->
            file:delete(AbsPath);
        true ->
            del_dir(AbsPath)
    end,
    del_dir(Dir, T);
del_dir(Dir, [_|T]) ->
    del_dir(Dir, T);
del_dir(Dir, []) ->
    file:del_dir(Dir).



ensure_dir(Path) when is_binary(Path) ->
    ensure_dir(unicode:characters_to_list(Path));
ensure_dir(Path) when is_list(Path) ->
    case filelib:ensure_dir(Path ++ "/") of
        ok -> filename:absname(Path);
        Err -> exit({ensure_dir, Err})
    end.



get_value_or_die(Key, PropList) ->
    case proplists:get_value(Key, PropList, '$$die$$') of
        '$$die$$' -> erlang:exit({no_value, Key});
        Value -> Value
    end.

command(Cmd) ->
    command(Cmd, "").

command(Cmd, Dir) ->
    command(Cmd, Dir, [], ?DEFAULT_TIMEOUT).

command(Cmd, Dir, Env, Timeout) ->
    CD = if Dir =:= "" -> [];
	    true -> [{cd, Dir}]
	 end,
    SetEnv = if Env =:= [] -> []; 
		true -> [{env, Env}]
	     end,
    Opt = CD ++ SetEnv ++ [stream, exit_status, use_stdio,
			   stderr_to_stdout, in, eof],
    P = open_port({spawn, Cmd}, Opt),
    get_port_data(P, [], Timeout).

get_port_data(P, D, Timeout) ->
    receive
	{P, {data, D1}} ->
	    get_port_data(P, [D1|D], Timeout);
	{P, eof} ->
	    port_close(P),    
	    receive
		{P, {exit_status, N}} ->
		    {N, normalize(lists:flatten(lists:reverse(D)))}
	    end
    after Timeout ->
        {110, "timeout"}
    end.

normalize([$\r, $\n | Cs]) ->
    [$\n | normalize(Cs)];
normalize([$\r | Cs]) ->
    [$\n | normalize(Cs)];
normalize([C | Cs]) ->
    [C | normalize(Cs)];
normalize([]) ->
    [].

-spec recursive_copy(list(), list()) -> ok.                            
recursive_copy(From, To) ->
    {ok, Files} = file:list_dir(From),
    [ok = rec_copy(From, To, X) || X <- Files],
    ok.
 
-spec rec_copy(list(), list(), list()) -> ok.                            
rec_copy(From, To, File) ->
    NewFrom = filename:join(From, File),
    NewTo   = filename:join(To, File),
    {ok, FI} = file:read_link_info(NewFrom),
    Type = FI#file_info.type,
    Mode = FI#file_info.mode,
    copy_by_type(Type, NewFrom, NewTo, Mode).

copy_by_type(Type, NewFrom, NewTo, Mode) ->
    case Type of
        directory  ->
            filelib:ensure_dir(NewTo++"/"),
            file:change_mode(NewTo, Mode),
            recursive_copy(NewFrom, NewTo);
        regular ->
            filelib:ensure_dir(NewTo),
            % {ok, Source} = file:open(NewFrom, [read]),
            % {ok, Dest} = file:open(NewTo, [write]),
            % file:copy(Source, Dest),
            % file:close(Source),
            % file:close(Dest),
            file:copy(NewFrom, NewTo),
            file:change_mode(NewTo, Mode);
        symlink ->
            {ok, SymPath} = file:read_link(NewFrom),
            file:make_symlink(SymPath, NewTo)
    end.


to_binary(List) when is_list(List) -> list_to_binary(List);
to_binary(Bin) when is_binary(Bin) -> Bin.


to_list(List) when is_list(List) -> List;
to_list(Bin) when is_binary(Bin) -> binary_to_list(Bin).
