-module(caterpillar_worker_plugin).

-export([behaviour_info/1]).


behaviour_info(callbacks) ->
    [
        {init_worker, 2},
        {changes, 3},
        {deploy, 3},
        {terminate_worker, 1}

    ];
behaviour_info(_) -> undefined.
