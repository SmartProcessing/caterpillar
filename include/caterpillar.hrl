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

-record(build_info, {
    state           :: success|error|none,
    package_spec    :: list(),
    description     :: list(),
    diff            :: list(),
    log             :: list(),
    test_info       :: list()
}).

-record(pkg_config, {
    name            :: binary(),
    dependencies    :: [#rev_def{}],
    platforms       :: [binary()],
    builders        :: [binary()],
    architectures   :: [binary()],
    maintainers     :: [binary()]
}).


-type version() :: 
    {
        Name        :: binary(),
        Branch      :: binary(),
        Tag         :: binary()
    }.

-type plugin_def() :: {atom(), [term()]}.

-type dependencie_record() :: {version(), atom(), [version()], [version()]}.
