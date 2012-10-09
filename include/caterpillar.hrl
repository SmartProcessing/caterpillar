-define(GV, proplists:get_value).
-define(VERSION, caterpillar_utils:get_version_by_revdef).


-record(archive_new, {
    name            :: binary(),
    branch          :: binary(),
    tag             :: binary(),
    revision        :: binary(),
    fd              :: file:io_device()|none
}).

-record(archive, {
    name            :: binary(),
    branch          :: binary(),
    archive         :: file:io_device()|none
}).


-record(rev_def, {
    name            :: binary(), 
    revision        :: binary(), 
    branch          :: binary(),
    tag             :: binary(),
    build_id        :: integer(),
    dep_object      :: [version()]
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
    dependencies    :: [version()],
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

%% @doc Dependencie record in dependencies dets table. 
%% {PackageVersion, {State, Bucket}, [DependsOn], [HasInDependencies]}
%% - PackageVersion - the version of the current build package;
%% - State - built|error - state of the lates build;
%% - Bucket - Folder identity where the package lays;
%% - DependsOn - Package versions current build depends;
%% - HasInDependencies - Package versions dependent from the current package.
-type dependencie_record() :: {version(), {built|error, [binary()]}, [version()], [version()]}.
