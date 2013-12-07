-ifndef(caterpillar_event_internal_hrl).
-define(caterpillar_event_internal_hrl, true).

-record(state, {
    ets :: ets:tab() % {{ref, pid}, worker|service, #ident{}|service_name, pid}
}).

-endif.
