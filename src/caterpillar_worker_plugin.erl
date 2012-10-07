-module(caterpillar_worker_plugin).

-export([behaviour_info/1]).


behaviour_info(callbacks) ->
    [
        {init_worker, 2},
        {handle_build, 2},
        {handle_deploy, 2},
        {handle_clean, 2},
        {terminate_worker, 1}

    ];
behaviour_info(_) -> undefined.
