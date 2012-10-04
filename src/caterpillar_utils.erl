-module(caterpillar_utils).

-include_lib("caterpillar.hrl").

-export([get_version_by_revdef/1, build_pipe/2]).
-export([pipe/3]).
-export([read_build_id/1, write_build_id/2]).
-export([package_to_archive/2, archive_to_package/1]).
-export([list_packages/1, del_dir/1]).
-export([ensure_dir/1]).


-type function_spec()   :: {function(), [term()]}.

-spec read_build_id(Filename::file:filename()) -> BuildId::pos_integer().
-spec write_build_id(Filename::file:filename(), BuildId::pos_integer()) -> ok | {error, Reason::term()}.



-spec get_version_by_revdef(RevDef :: #rev_def{}) -> Version :: version().
get_version_by_revdef(RevDef) ->
    Name = RevDef#rev_def.name,
    Branch = RevDef#rev_def.branch,
    Tag = RevDef#rev_def.tag,
    {Name, Branch, Tag}.

-spec build_pipe([function_spec()], term()) -> {ok, term()} | {error, term()}.
build_pipe(Funs, Init) ->
    lists:foldl(
        fun({Function, Opts}, Acc) ->
            {ok, Res} = erlang:apply(Function, [Opts, Acc]),
            Res
        end, Init, Funs).


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



read_build_id(Filename) ->
    check_build_id_file(Filename),
    case file:consult(Filename) of
        {ok, [{build_id, BuildId}]} when is_integer(BuildId) ->
            BuildId;
        _ ->
            write_build_id(Filename, 1),
            1
    end.


write_build_id(Filename, BuildId) when is_integer(BuildId)->
    file:write_file(
        Filename,
        io_lib:format("~p.", [{build_id, BuildId}])
    );
write_build_id(_, BuildId) -> 
    error_logger:error_msg("write_build_id: bad build_id - ~p~n", [BuildId]),
    {error, badarg}.


check_build_id_file(Filename) ->
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


del_dir(Dir, [H|T]) when H /= "." andalso H /= ".." ->
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



ensure_dir(Path) ->
    filelib:ensure_dir(Path ++ "/"), Path.
