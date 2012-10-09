-ifndef(caterpillar_event_internal).
-define(caterpillar_event_internal, true).

-record(state, {
    ets :: ets:tab()
}).


-record(scan_pipe_result, {
    notity :: NotifyRecord::record(),
    archives :: [ArchiveRecord::record()],
    packaeges :: [PackageRecord::record()]
}).


-endif.
