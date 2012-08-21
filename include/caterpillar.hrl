-define(GV, proplists:get_value).
-record(rev_def, {
    name            :: binary(), 
    revision        :: binary(), 
    branch          :: binary(),
    tag             :: binary(),
    build_id        :: integer(),
    dep_subject     :: list(),
    platform_spec   :: term()
}).
