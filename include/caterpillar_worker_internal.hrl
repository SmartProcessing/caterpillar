-ifndef(caterpillar_worker_internal).
-define(caterpillar_worker_internal, true).


-define(ARCHIVE_ROOT, "").
-define(REPOSITORY_ROOT, "").


-record(state, {
    ident :: atom(),
    worker_plugin :: atom(),
    worker_state :: term(),
    archive_root :: filelib:dirname(),
    repository_root :: filelib:dirname()
}).


-endif.
