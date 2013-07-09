-module(caterpillar_utils).

-include_lib("caterpillar.hrl").
-include_lib("caterpillar_builder_internal.hrl").
-include_lib("kernel/include/file.hrl").

-export([get_version_by_revdef/1, build_pipe/2]).
-export([pipe/3]).
-export([read_work_id/1, write_work_id/2]).
-export([package_to_archive/2, archive_to_package/1]).
-export([list_packages/1, del_dir/1, recursive_copy/2]).
-export([ensure_dir/1]).
-export([get_value_or_die/2]).
-export([command/1, command/2, command/4]).
-export([filename_join/1, filename_join/2]).
-export([gen_ident/2, any_ident/0]).


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
    string:join([Repo, Branch], "__ARCHIVE__").


archive_to_package(Archive) ->
    [Repo, Branch] = string:tokens(Archive, "__ARCHIVE__"),
    {Repo, Branch}.



-spec list_packages(Path::filelib:dirname()) -> {ok, [filename:name()]}|{error, Reason::term()}.
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
    list_packages(O, [H|Accum]).


del_dir(Dir) ->
    case filelib:is_dir(Dir) of
        true ->
            {ok, All} = file:list_dir(Dir),
            del_dir(Dir, All);
        false ->
            file:delete(Dir) 
    end.


del_dir(Dir, [H|T]) when H /= <<".">> andalso H /= <<"..">> ->
    AbsPath = filename_join(Dir, H),
    case filelib:is_dir(AbsPath) of
        false ->
            file:delete(AbsPath);
        true ->
            del_dir(AbsPath)
    end,
    del_dir(Dir, T);
del_dir(Dir, [_|T]) -> del_dir(Dir, T);
del_dir(Dir, []) -> file:del_dir(Dir).


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


-spec command(Cmd::string()) -> {Code::integer(), Result::string()}.
command(Cmd) -> command(Cmd, []).


command(Cmd, Options) -> command(Cmd, Options, [], ?DEFAULT_TIMEOUT).


command(Cmd__, Options, Env, Timeout) ->
    Cmd = lists:flatten(Cmd__),
    Dir = ?GV(cwd, Options, ""),
    CD = if
        Dir =:= "" -> [];
	    true -> [{cd, Dir}]
    end,
    SetEnv = if
        Env =:= [] -> []; 
		true -> [{env, Env}]
    end,
    %FIXME: binary?
    DefaultOpts = [stream, exit_status, use_stdio, stderr_to_stdout, in],
    PortOptions = CD ++ SetEnv ++ DefaultOpts,
    Port = open_port({spawn, Cmd}, PortOptions),
    get_port_data(Port, [], Timeout).


get_port_data(Port, Accum, Timeout) ->
    receive
        {_Port, {data, Chunk}} ->
            get_port_data(Port, [Chunk|Accum], Timeout); 
        {_Port, {exit_status, ExitStatus}} ->
            {ExitStatus, lists:flatten(lists:reverse(Accum))}
    after Timeout ->
        erlang:port_close(Port),
        {110, "timeout"}
    end.


-spec recursive_copy(list(), list()) -> ok.                            
recursive_copy(From, To) ->
    {ok, Files} = file:list_dir(From),
    [ok = rec_copy(From, To, X) || X <- Files],
    ok.
 

-spec rec_copy(list(), list(), list()) -> ok.                            
rec_copy(From, To, File) ->
    NewFrom = filename_join(From, File),
    NewTo   = filename_join(To, File),
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


to_list(List) when is_list(List) -> List;
to_list(Bin) when is_binary(Bin) -> binary_to_list(Bin);
to_list(Atom) when is_atom(Atom) -> atom_to_list(Atom).


filename_join(First, Second) ->
    filename_join([First, Second]).


filename_join(Names) -> unicode:characters_to_list(filename_join_(Names, [])).


filename_join_([Name|[]], []) -> to_list(Name);
filename_join_([], Accum) -> lists:flatten(lists:reverse(Accum));
filename_join_([Name|Other], []) -> filename_join_(Other, ["/", to_list(Name)|[]]);
filename_join_([Name|[]], Accum) -> filename_join_([], [to_list(Name)|Accum]);
filename_join_([Name|Other], Accum) -> filename_join_(Other, ["/", to_list(Name)|Accum]).


-spec gen_ident(Type::atom(), Arch::atom()) -> #ident{}.
gen_ident(Type, Arch) ->
    ToBin = fun
        (X) when is_atom(X) -> atom_to_binary(X, latin1);
        (X) when is_list(X) -> list_to_binary(X);
        (X) when is_binary(X) -> X;
        (X) -> list_to_binary(io_lib:format("~p", [X]))
    end,
    #ident{type=ToBin(Type), arch=ToBin(Arch)}.


any_ident() -> '_'.
