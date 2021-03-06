-ifndef(caterpillar_repository_internal).
-define(caterpillar_repository_internal, true).

-define(REPOSITORY_LIBPATH, "/var/lib/caterpillar/repository/").
-define(ARCHIVE_ROOT, filename:join(?REPOSITORY_LIBPATH, "archive")).
-define(EXPORT_ROOT, filename:join(?REPOSITORY_LIBPATH, "export")).
-define(NOTIFY_ROOT, filename:join(?REPOSITORY_LIBPATH, "notify")).
-define(DETS, filename:join(?REPOSITORY_LIBPATH, "repository.db")).
-define(WORK_ID_FILE, filename:join(?REPOSITORY_LIBPATH, "work_id_file")).
-define(REPOSITORY_ROOT, "/srv/repository").
-define(SCAN_INTERVAL, 600).
-define(CLEANUP_INTERVAL, 600).

-define(REPOSITORY_DB_VERSION, "0.2").


-type work_id() :: non_neg_integer().


-record(state, {
    work_id :: work_id(),
    work_id_file :: filelib:filename(),
    ets :: ets:tab(),
    dets :: dets:tab(),
    archive_root :: filelib:dirname(),
    repository_root :: filelib:dirname(),
    notify_root :: filelib:dirname(),
    vcs_plugin :: atom(),
    vcs_state :: term(),
    scan_timer :: reference(),
    registered = false :: boolean(),
    register_service_timer :: reference(),
    scan_interval = ?SCAN_INTERVAL :: non_neg_integer(),
    cleanup_interval :: non_neg_integer(),
    cleanup_timer :: reference()
}).


-record(repository_package, {
    name :: string(),
    branch :: string(),
    tag :: term(),
    status = ok :: ok | error,
    failed_at = none :: term(),
    reason :: term(),
    old_revno = none :: term(),
    current_revno :: term(),
    archive_name :: string(),
    archive_type :: term(),
    diff = <<>> :: binary(),
    changelog = <<>> :: binary()
}).


-record(changes, {
    notify :: NotifyRecord::record(),
    archives :: [ArchiveRecord::record()],
    packages :: [PackageRecord::record()]
}).

-endif.
