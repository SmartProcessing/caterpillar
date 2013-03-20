-module(caterpillar_dependencies).
-include_lib("caterpillar_internal.hrl").
-export([check_intersection/2, list_unresolved_dependencies/3]).
-export([update_dependencies/3, fetch_dependencies/2]).

-define(LOCK(X), gen_server:call(caterpillar_lock, {lock, X}, infinity)).
-define(UNLOCK(X), gen_server:call(caterpillar_lock, {unlock, X})).

-spec list_unresolved_dependencies(reference(), #rev_def{}, [version()]) ->
    {ok, Unresolved :: [version()]}.
list_unresolved_dependencies(DepTree, Candidate, Preparing) ->
    Deps = Candidate#rev_def.dep_object,
    list_none_new_dependencies(DepTree, Deps, Preparing).

list_none_new_dependencies(DepTree, Deps, Preparing) ->
    list_none_new_dependencies(DepTree, Deps, Preparing, {[], []}).

list_none_new_dependencies(_DepTree, [], _Preparing, {NoneDeps, NewDeps}) ->
    {ok, NoneDeps, NewDeps};
list_none_new_dependencies(DepTree, [{Dep, State}|O], Preparing, {NoneDeps, NewDeps}) ->
    case fetch_dependencies(DepTree, Dep) of
        {ok, {_VersionSpec, {none, _B}, _Obj, _Subj}} ->
            list_none_new_dependencies(DepTree, O, Preparing, {[Dep|NoneDeps], NewDeps});
        {ok, {_VersionSpec, {new, _B}, _Obj, _Subj}} ->
            list_none_new_dependencies(DepTree, O, Preparing, {NoneDeps, [Dep|NewDeps]});
        {ok, {_VersionSpec, {Success, _B}, _Obj, _Subj}} when State == <<"new">> ->
            list_none_new_dependencies(DepTree, O, Preparing, {NoneDeps, NewDeps});
        {ok, {_VersionSpec, {Success, _B}, _Obj, _Subj}} when Success == <<"built">>; Success == State ->
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


-spec check_intersection(#rev_def{}, [#rev_def{}]) -> 
    {ok, independent|dependent}.
%% @doc Check whether the packages that are building now 
%% have in dependecies or depends on current build candidate
check_intersection(Candidate, CheckList) ->
    CandidateVersion = ?VERSION(Candidate),
    CandidateDeps = [{N, B, T} || {{N, B, T}, S} <- Candidate#rev_def.dep_object],
    Intersect = lists:foldl(
        fun(BuildUnit, Res) ->
            BuildVersion = ?VERSION(BuildUnit), 
            BuildDeps = [{N, B, T} || {{N, B, T}, S} <- BuildUnit#rev_def.dep_object],
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

-spec fetch_dependencies(DepTree :: reference(), Version :: version()) ->
    {ok, dependencie_record()} | {error, Reason :: atom()}.
fetch_dependencies(DepTree, Version) ->
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

-spec update_dependencies(reference(), #rev_def{}, atom()) -> 
    {ok, done}|{error, Reason :: atom()}.
update_dependencies(DepTree, Rev, Status) ->
    error_logger:info_msg("updating dependencies: Rev: ~p Status: ~p~n", [Rev, Status]),
    Version = ?VERSION(Rev),
    DepObject = Rev#rev_def.dep_object, 
    update_subjects(DepTree, DepObject, Version),
    ?LOCK(Version),
    NewObj = case dets:lookup(DepTree, Version) of
        [{V, {_, Buckets}, _, Subj}|_] when V == Version ->
            {Version, {Status, Buckets}, DepObject, Subj};
        [] ->
            {Version, {Status, []}, DepObject, []}
    end,
    dets:insert(DepTree, NewObj),
    ?UNLOCK(Version),
    {ok, done}.


update_subjects(_, [], _) ->
    {ok, done};
update_subjects(Deps, [Version|Other], NewRef) ->
    ?LOCK(Version),
    [{Version, BuildInfo, Object, Subject}|_] = dets:lookup(Deps, Version),
    case lists:member(NewRef, Subject) of
        true ->
            ok;
        false ->
            dets:insert(Deps, {Version, BuildInfo, Object, [NewRef|Subject]})
    end,
    ?UNLOCK(Version),
    update_subjects(Deps, Other, NewRef).
