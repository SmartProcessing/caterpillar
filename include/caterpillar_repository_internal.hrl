-ifndef(caterpillar_repository_internal).
-define(caterpillar_repository_internal, true).

-define(ARCHIVE_ROOT, "/var/lib/caterpillar/repository/archive").
-define(EXPORT_ROOT, "/var/lib/caterpillar/repository/export").
-define(REPOSITORY_ROOT, "/srv/repository").
-define(SCAN_INTERVAL, 600).
-define(DETS, "/var/lib/caterpillar/repository/repository.db").

-record(state, {
    ets :: ets:tab(),
    dets :: dets:tab(),
    archive_root :: string(),
    export_root :: string(),
    repository_root :: string(),
    vcs_plugin :: atom(),
    vcs_state :: term(),
    scan_timer :: reference(),
    scan_interval :: non_neg_integer()
}).

-record(package, {
    name :: string(),
    branch :: string(),
    old_revno = none :: term(),
    current_revno :: term(),
    archive :: string(),
    diff = <<>> :: binary(),
    changelog = <<>> :: binary()
}).

-endif.
