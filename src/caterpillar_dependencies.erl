-module(caterpillar_dependencies).
-include("caterpillar.hrl").
-export([check_intersection/3, list_unresolved_dependencies/2]).

-type dependencie_record() :: {version(), atom(), [version()], [version()]}.

-spec list_unresolved_dependencies(reference(), #rev_def{}) ->
    {ok, Unresolved :: list()}.
list_unresolved_dependencies(DepTree, Candidate) ->
    Deps = Candidate#rev_def.dep_subject,
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
check_intersection(DepTree, Candidate, CheckList) ->
    CandidateVersion = ?VERSION(Candidate),
    Intersect = lists:foldl(
        fun(BuildUnit, Res) ->
            BuildVersion = ?VERSION(BuildUnit), 
            case fetch_dependencies(DepTree, BuildVersion) of
                {ok, {BuildVersion, _State, Object, Subject}} ->
                    Res or 
                        lists:member(CandidateVersion, Object) or
                        lists:member(CandidateVersion, Subject);
                _Other ->
                    %% fall dear, something that shouldn't be built is building
                    {error, no_building_rev_info}
            end
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
            {error, no_rev_info};
        _Other ->
            {error, dets_error}
    end.
