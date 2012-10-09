-module(caterpillar_repository_plugin).

-export([behaviour_info/1]).


behaviour_info(callbacks) ->
    [
        {init_plugin, 1},
        {terminate_plugin, 1},
        {get_tag, 4},
        {get_diff, 5},
        {get_changelog, 5},
        {get_revno, 3},
        {is_repository, 2},
        {is_branch, 3},
        {get_branches, 2},
        {export, 5}
    ];
behaviour_info(_) -> undefined.
