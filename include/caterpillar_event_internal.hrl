-ifndef(caterpillar_event_internal).
-define(caterpillar_event_internal, true).

-record(state, {
    ets :: ets:tab()
}).

-endif.
