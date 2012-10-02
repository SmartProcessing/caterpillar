-module(caterpillar_repository_plugin).

-export([behaviour_info/1]).


behaviour_info(callbacks) ->
    [
        {init, 1},
        {terminate, 0},
        {checkout_branch, 3},
        {get_diff, 5},
        {get_changelog, 5},
        {get_revno, 3},
        {is_repository, 2},
        {is_branch, 3},
        {get_branches, 2},
        {export_branch, 4}
    ];
behaviour_info(_) -> undefined.
