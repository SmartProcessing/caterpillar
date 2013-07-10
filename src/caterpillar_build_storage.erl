-module(caterpillar_build_storage).
-include_lib("caterpillar_builder_internal.hrl").
-export([check_isect/2, list_unres_deps/3, cleanup_new_in_progress/1]).
-export([update_dep_state/3, fetch_dep/2, fetch_dep_non_block/2, create_dep/2, get_subj/2]).
-export([arm_bucket/4]).
-export([delete/3, get_path/3, get_statpack_path/3, get_temp_path/2, delete_statpack/2]).
-export([store_package_with_state/3]).

-define(LOCK(X), gen_server:call(caterpillar_lock, {lock, X}, infinity)).
-define(UNLOCK(X), gen_server:call(caterpillar_lock, {unlock, X})).
-define(CPU, caterpillar_pkg_utils).
-define(CU, caterpillar_utils).
-define(BTL, binary_to_list).

-spec list_unres_deps(reference(), #rev_def{}, [version()]) ->
    {ok, Unresolved :: [version()]}.
list_unres_deps(DepTree, Candidate, Preparing) ->
    Deps = Candidate#rev_def.dep_object,
    list_none_new_dependencies(DepTree, Deps, Preparing).

list_none_new_dependencies(DepTree, Deps, Preparing) ->
    list_none_new_dependencies(DepTree, Deps, Preparing, {[], []}).

list_none_new_dependencies(_DepTree, [], _Preparing, {NoneDeps, NewDeps}) ->
    {ok, NoneDeps, NewDeps};
list_none_new_dependencies(DepTree, [{Dep, State}|O], Preparing, {NoneDeps, NewDeps}) ->
    case fetch_dep(DepTree, Dep) of
        {ok, {_VersionSpec, <<"missing">>, _Obj, _Subj}} ->
            list_none_new_dependencies(DepTree, O, Preparing, {[Dep|NoneDeps], NewDeps});
        {ok, {_VersionSpec, _Success, _Obj, _Subj}} when State == <<"new">> ->
            list_none_new_dependencies(DepTree, O, Preparing, {NoneDeps, NewDeps});
        {ok, {_VersionSpec, <<"none">>, _Obj, _Subj}} ->
            list_none_new_dependencies(DepTree, O, Preparing, {[Dep|NoneDeps], NewDeps});
        {ok, {_VersionSpec, NewInProgress, _Obj, _Subj}} when NewInProgress == <<"new">>; NewInProgress == <<"in_progress">> ->
            list_none_new_dependencies(DepTree, O, Preparing, {NoneDeps, [Dep|NewDeps]});
        {ok, {_VersionSpec, Success, _Obj, _Subj}} when Success == <<"built">>; Success == State ->
            list_none_new_dependencies(DepTree, O, Preparing, {NoneDeps, NewDeps});
        {ok, []} ->
            case lists:member(Dep, Preparing) of
                true ->
                    list_none_new_dependencies(DepTree, O, Preparing, {NoneDeps, [Dep|NewDeps]});
                false ->
                    list_none_new_dependencies(DepTree, O, Preparing, {[Dep|NoneDeps], NewDeps})
            end;
        _Other ->
            list_none_new_dependencies(DepTree, O, Preparing, {[Dep|NoneDeps], NewDeps})
    end.


-spec check_isect(#rev_def{}, [#rev_def{}]) -> 
    {ok, independent|dependent}.
%% @doc Check whether the packages that are building now 
%% have in dependecies or depends on current build candidate
check_isect(Candidate, CheckList) ->
    CandidateVersion = ?VERSION(Candidate),
    CandidateDeps = [{N, B, T} || {{N, B, T}, _S} <- Candidate#rev_def.dep_object],
    Intersect = lists:foldl(
        fun(BuildUnit, Res) ->
            BuildVersion = ?VERSION(BuildUnit), 
            BuildDeps = [{N, B, T} || {{N, B, T}, _S} <- BuildUnit#rev_def.dep_object],
            IsDepObject = lists:member(CandidateVersion, BuildDeps),
            IsDepSubject = lists:member(BuildVersion, CandidateDeps),
            IsBuilding = BuildVersion =:= CandidateVersion,
            Res or IsDepObject or IsDepSubject or IsBuilding
        end, false, CheckList),
    case Intersect of
        true ->
            {ok, dependent};
        false ->
            {ok, independent}
    end.

-spec fetch_dep(DepTree :: reference(), Version :: version()) ->
    {ok, dependencie_record()} | {error, Reason :: atom()}.
fetch_dep(DepTree, Version) ->
    ?LOCK(Version),
    Res = case catch dets:lookup(DepTree, Version) of
        [{BuildVersion, State, Object, Subject}|_] ->
            {ok, {BuildVersion, State, Object, Subject}};
        [] ->
            {ok, []};
        _Other ->
            {error, dets_error}
    end,
    ?UNLOCK(Version),
    Res.

fetch_dep_non_block(DepTree, Version) ->
    case catch dets:lookup(DepTree, Version) of
        [{BuildVersion, State, Object, Subject}|_] ->
            {ok, {BuildVersion, State, Object, Subject}};
        [] ->
            {ok, []};
        _Other ->
            {error, dets_error}
    end.

get_subj(DepTree, Version) ->
    case fetch_dep(DepTree, Version) of
        {ok, {_, _, _, Subj}} ->
            Subj;
        _Other ->
            {error, not_found}
    end.

create_dep(DepTree, Rev) ->
    Version = ?VERSION(Rev),
    DepObject = Rev#rev_def.dep_object, 
    update_subjects(DepTree, DepObject, Version),
    ?LOCK(Version),
    case dets:lookup(DepTree, Version) of
        [{_V, <<"missing">>, _, Subj}|_] ->
            dets:insert(DepTree, {Version, <<"new">>, DepObject, Subj});
        [] ->
            dets:insert(DepTree, {Version, <<"new">>, DepObject, []});
        [{_V, {_, _Buckets}, _, _Subj}|_] ->
            pass
    end,
    ?UNLOCK(Version),
    {ok, done}.

-spec update_dep_state(reference(), #rev_def{}, binary()) -> 
    {ok, done}|{error, Reason :: atom()}.
update_dep_state(DepTree, Rev, Status) ->
    Version = ?VERSION(Rev),
    DepObject = Rev#rev_def.dep_object, 
    update_subjects(DepTree, DepObject, Version),
    ?LOCK(Version),
    NewObj = case dets:lookup(DepTree, Version) of
        [{V, _, _, Subj}|_] when V == Version ->
            {Version, Status, DepObject, Subj};
        [] ->
            {Version, Status, DepObject, []}
    end,
    dets:insert(DepTree, NewObj),
    ?UNLOCK(Version),
    {ok, done}.


update_subjects(_, [], _) ->
    {ok, done};
update_subjects(Deps, [{Version, _}|Other], NewRef) ->
    ?LOCK(Version),
    case dets:lookup(Deps, Version) of
        [{Version, BuildInfo, Object, Subject}|_] ->
            dets:insert(Deps, {Version, BuildInfo, Object, lists:usort([NewRef|Subject])});
        [] ->
            dets:insert(Deps, {Version, <<"missing">>, [], [NewRef]})
    end,
    ?UNLOCK(Version),
    update_subjects(Deps, Other, NewRef).

delete(Deps, Path, Version) ->
    {Name, _, _} = Version,
    dets:delete(Deps, Version),
    StatDel = fun(X) ->
        catch ?CU:del_dir(get_path(Path, Version, X))
    end,
    lists:map(StatDel, [<<"new">>, <<"tested">>, <<"built">>]).

arm_bucket(Rev, _Deps, BuildPath, []) ->
    RevPath = get_path(BuildPath, Rev, <<"new">>),
    copy_package_to_bucket(RevPath, 
        filename:join([get_statpack_path(BuildPath, Rev, Rev#rev_def.work_id), ?BTL(Rev#rev_def.name)])),
    {ok, done};
arm_bucket(Rev, Deps, BuildPath, [Dependencie|O]) ->
    {Dep={Name, _, _}, BState} = Dependencie,
    DepPath = get_path(BuildPath, Dep, BState),
    copy_package_to_bucket(DepPath, 
        filename:join([get_statpack_path(BuildPath, Rev, Rev#rev_def.work_id), ?BTL(Name)])),
    arm_bucket(Rev, Deps, BuildPath, O).

copy_package_to_bucket(Source, Path) ->
    error_logger:info_msg("copying ~p to ~p~n", [Source, Path]),
    ?CU:del_dir(Path),
    filelib:ensure_dir(Path),
    ?CU:recursive_copy(Source, Path).

cleanup_new_in_progress(Deps) ->
    lists:map(fun(X) -> update_new_in_progress(Deps, X) end, dets:match(Deps, '$1')).

update_new_in_progress(Deps, [{Vsn, {State, InB}, Subj, Obj}]) when State == <<"in_progress">> ->
    dets:insert(Deps, {Vsn, {<<"none">>, InB}, Subj, Obj});
update_new_in_progress(Deps, _) ->
    ok.

-spec delete_statpack(#rev_def{}, list()) -> ok.
delete_statpack(Rev, BPath) ->
    ?CU:del_dir(get_statpack_path(BPath, Rev, Rev#rev_def.work_id)).

-spec store_package_with_state(binary(), #rev_def{}, list()) -> ok|pass.
store_package_with_state(<<"none">>, _, _) ->
    pass;
store_package_with_state(State, Rev, BPath) ->
    Src = filename:join([get_statpack_path(BPath, Rev, Rev#rev_def.work_id), ?BTL(Rev#rev_def.name)]),
    Dst = get_path(BPath, Rev, State),
    copy_package_to_bucket(Src, Dst).

-spec get_temp_path(list(), version()|#rev_def{}) -> list().
get_temp_path(Prefix, Rev) ->
    TmpName = filename:join([Prefix, "temp", get_dir_name(Rev, "tmp")]),
    filelib:ensure_dir(TmpName),
    TmpName.

-spec get_path(list(), version(), list()|binary()) -> list().
get_path(Prefix, Vsn, State) ->
    Dir = filename:join([Prefix, "pstore", get_dir_name(Vsn, State)]),
    filelib:ensure_dir(Dir ++ "/empty"),
    Dir.

-spec get_statpack_path(list(), version(), integer()) -> list().
get_statpack_path(Prefix, RV, WorkId) ->
    Dir = filename:join([Prefix, "buckets", get_dir_name(RV, integer_to_list(WorkId))]),
    filelib:ensure_dir(Dir),
    Dir.

get_dir_name(RV) ->
    get_dir_name(RV, <<"new">>).

get_dir_name(Rev=#rev_def{}, Postfix) ->
    Name = ?BTL(Rev#rev_def.name),
    Branch = ?BTL(Rev#rev_def.branch),
    Tag = ?BTL(Rev#rev_def.tag),
    lists:flatten(io_lib:format(
        "~s-~s-~s.~s", [Name, Branch, Tag, Postfix]));

get_dir_name({N, B, T}, Postfix) ->
    Name = ?BTL(N),
    Branch = ?BTL(B),
    Tag = ?BTL(T),
    lists:flatten(io_lib:format(
        "~s-~s-~s.~s", [Name, Branch, Tag, Postfix])).
