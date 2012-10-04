-ifndef(caterpillar_repository_internal).
-define(caterpillar_repository_internal, true).

-define(REPOSITORY_LIBPATH, "/var/lib/caterpillar/repository/").
-define(ARCHIVE_ROOT, filename:join(?REPOSITORY_LIBPATH, "archive")).
-define(EXPORT_ROOT, filename:join(?REPOSITORY_LIBPATH, "export")).
-define(DETS, filename:join(?REPOSITORY_LIBPATH, "repository.db")).
-define(BUILD_ID_FILE, filename:join(?REPOSITORY_LIBPATH, "build_id_file")).
-define(REPOSITORY_ROOT, "/srv/repository").
-define(SCAN_INTERVAL, 600).


-record(state, {
    build_id_file :: filelib:filename(),
    ets :: ets:tab(),
    dets :: dets:tab(),
    archive_root :: filelib:dirname(),
    export_root :: filelib:dirname(),
    repository_root :: filelib:dirname(),
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
