-module(caterpillar_dependencies).
-export([check_list/3]).

-spec check_list(dets(), caterpillar:revdef(), [caterpillar:revdef()]) ->
    {ok, [{depends, []}, {is_dependencie, []}]}.
check_list(DepTree, Candidate, [InProgress]) ->
    %% TODO
    {error, not_implemented}.
