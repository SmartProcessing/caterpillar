-ifndef(caterpillar_repository_internal).
-define(caterpillar_repository_internal, true).

-define(ARCHIVE_PATH, "/var/lib/caterpillar/repository/archive").

-record(state, {
    ets :: ets:tab(),
    dets :: dets:tab(),
    repository_root :: string(),
    archive_root :: string(),
    scan_timer :: reference(),
    scan_timer_interval = 600 :: non_neg_integer()
}).

-endif.
