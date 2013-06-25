-type version() :: {binary(), binary()}.

-type package_description() :: 
{version(), description(), [build_id()]}.

-type build_record() :: 
{
    {build_id(), version()},
    state(),
    smprc_datetime_utils:datetime(),
    smprc_datetime_utils:datetime(),
    commit_hash(),
    build_log(),
    package_name()}.

-type build_id() :: integer().

-type commit_hash() :: binary().

-type build_log() :: binary().

-type state() :: binary().

-type description() :: binary().

-type package_name() :: binary().
