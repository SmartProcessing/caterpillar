-module(caterpillar_dependencies).
-export([check_list/3]).

-spec check_list(pid(), caterpillar:revdef(), [caterpillar:revdef()]) -> 
    {ok, {depends, []}, {is_dependencie, []}}.
check_list(_DepTree, _Candidate, _CheckList) ->
    %% TODO
    {error, not_implemented}.
