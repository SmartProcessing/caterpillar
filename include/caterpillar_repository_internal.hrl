-ifndef(caterpillar_repository_internal).
-define(caterpillar_repository_internal, true).

-define(ARCHIVE_PATH, "/var/lib/caterpillar/repository/archive").
-define(SCAN_INTERVAL, 600).

-record(state, {
    ets :: ets:tab(),
    dets :: dets:tab(),
    repository_root :: string(),
    archive_root :: string(),
    vcs_plugin :: atom(),
    vcs_state :: term(),
    scan_timer :: reference(),
    scan_interval :: non_neg_integer()
}).

-endif.
