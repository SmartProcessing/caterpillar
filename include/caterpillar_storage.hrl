-type version() :: {binary(), binary()}.

-type package_description() :: 
{{ident(), version()}, description(), [build_id()]}.

-type build_record() :: 
{
    {ident(), build_id(), version()},
    state(),
    smprc_datetime_utils:datetime(),
    smprc_datetime_utils:datetime(),
    commit_hash(),
    build_log(),
    package_name()
}.

-type ident() :: atom().

-type build_id() :: integer().

-type commit_hash() :: binary().

-type build_log() :: binary().

-type state() :: binary().

-type description() :: binary().

-type package_name() :: binary().
