-module(caterpillar_utils).

-include_lib("caterpillar.hrl").

-export([get_version_by_revdef/1, build_pipe/2]).
-export([pipe/3]).
-export([read_build_id/1, write_build_id/2]).
-export([branch_to_archive/2, archive_to_branch/1]).
-export([list_dir/1]).


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



branch_to_archive(Repo, Branch) ->
    string:join([Repo, Branch], "__ARCHIVE__").


archive_to_branch(Archive) ->
    [Repo, Branch] = string:tokens(Archive, "__ARCHIVE__"),
    {Repo, Branch}.



list_dir(Path) ->
    {ok, List} = file:list_dir(Path),
    list_dir(List, []).


list_dir([], Accum) ->
    {ok, Accum};
list_dir([ [$.|_]|O ], Accum) ->
    list_dir(O, Accum);
list_dir([ H|O ], Accum) ->
    list_dir(O, [H|Accum]).

