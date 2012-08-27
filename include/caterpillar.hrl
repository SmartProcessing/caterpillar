-define(GV, proplists:get_value).
-define(VERSION, caterpillar_utils:get_version_by_revdef).

-record(rev_def, {
    name            :: binary(), 
    revision        :: binary(), 
    branch          :: binary(),
    tag             :: binary(),
    build_id        :: integer(),
    dep_object      :: [version()],
    platform_spec   :: term()
}).

-type version() :: 
    {
        Name        :: binary(),
        Branch      :: binary(),
        Tag         :: binary()
    }.


