-module(caterpillar_dependencies).
-include("caterpillar.hrl").
-export([check_list/3, check_consistency/2]).

-spec check_consistency(reference(), #rev_def{}) ->
    {ok, success} | {ok, unresolved, _List} | {error, error}.
check_consistency(DepTree, Candidate) ->
    Deps = Candidate#rev_def.dep_subject,
    case lists:filter(
        fun(X) ->
            {_VersionSpec, State, _Obj, _Subj} = dets:lookup(DepTree, X),
            State == built
        end, Deps) of
        [] ->
            {ok, success};
        Deps when is_list(Deps) ->
            {ok, unresolved, Deps};
        _Other ->
            {error, error}
    end.

-spec check_list(reference(), #rev_def{}, [#rev_def{}]) -> 
    {ok, independent|dependent}.
check_list(_DepTree, _Candidate, _CheckList) ->
    %%TODO
    {error, not_implemented}.
