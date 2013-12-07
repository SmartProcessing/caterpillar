-ifndef(caterpillar_worker_internal).
-define(caterpillar_worker_internal, true).


-record(state, {
    worker_pid :: pid(),
    ident :: atom(),
    registered=false :: boolean(),
    worker_plugin :: atom(),
    worker_state :: term(),
    archive_root :: filelib:dirname(),
    repository_root :: filelib:dirname()
}).


-endif.
