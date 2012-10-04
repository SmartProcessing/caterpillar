-module(caterpillar_dependencies).
-include_lib("caterpillar.hrl").
-export([check_intersection/3, list_unresolved_dependencies/2]).
-export([update_dependencies/2]).

-spec list_unresolved_dependencies(reference(), #rev_def{}) ->
    {ok, Unresolved :: [version()]}.
list_unresolved_dependencies(DepTree, Candidate) ->
    Deps = Candidate#rev_def.dep_object,
    {ok, lists:filter(
        fun(X) ->
            case fetch_dependencies(DepTree, X) of
                {ok, {_VersionSpec, State, _Obj, _Subj}} ->
                    State /= built;
                _Other ->
                    true
            end
        end, Deps)}.

-spec check_intersection(reference(), #rev_def{}, [#rev_def{}]) -> 
    {ok, independent|dependent}.
%% @doc Check whether the packages that are building now 
%% have in dependecies or depends on current build candidate
check_intersection(DepTree, Candidate, CheckList) ->
    CandidateVersion = ?VERSION(Candidate),
    CandidateDeps = Candidate#rev_def.dep_object,
    Intersect = lists:foldl(
        fun(BuildUnit, Res) ->
            BuildVersion = ?VERSION(BuildUnit), 
            BuildDeps = BuildUnit#rev_def.dep_object,
            IsDepObject = lists:member(CandidateVersion, BuildDeps),
            IsDepSubject = lists:member(BuildVersion, CandidateDeps),
            Res or IsDepObject or IsDepSubject
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
    case catch dets:lookup(DepTree, Version) of
        [{BuildVersion, State, Object, Subject}|_] ->
            {ok, {BuildVersion, State, Object, Subject}};
        [] ->
            {ok, []};
        _Other ->
            {error, dets_error}
    end.


-spec update_dependencies(reference(), #rev_def{}) -> 
    {ok, done}|{error, Reason :: atom()}.
update_dependencies(DepTree, Rev) ->
    Version = ?VERSION(Rev),
    DepObject = Rev#rev_def.dep_object, 
    update_subjects(DepTree, DepObject, Version),
    DepSubject = case dets:lookup(DepTree, Version) of
        [{Version, _, _, Subj}|_] ->
            Subj;
        _Other ->
            []
    end,
    dets:insert(DepTree,
        {Version, built, DepObject, DepSubject}),
    {ok, done}.


update_subjects(_, [], _) ->
    {ok, done};
update_subjects(DepTree, [Version|Other], NewRef) ->
    [{Version, BuildInfo, Object, Subject}|_] = dets:lookup(DepTree, Version),
    case lists:member(NewRef, Object) of
        true ->
            ok;
        false ->
            dets:insert(DepTree, {Version, BuildInfo, Object, Subject})
    end,
    update_subjects(DepTree, Other, NewRef).
