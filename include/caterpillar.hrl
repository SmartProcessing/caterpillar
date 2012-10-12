-define(GV, proplists:get_value).
-define(VERSION, caterpillar_utils:get_version_by_revdef).


-record(archive, {
    name            :: string(),
    branch          :: string(),
    tag             :: term(),
    archive_name    :: filelib:filename(),
    fd              :: file:io_device()
}).

-record(notify, {
    subject = <<>> :: binary,
    body = <<>> :: binary
}).

%FIXME: move to internal hrl
-record(pkg_config, {
    name                :: string(),
    version="0.0.0"     :: string(),
    deps=[]             :: [version()|string()],
    build_deps=[]       :: [version()|string()],
    section="smprc"     :: string(),
    platform="default"  :: string(),
    package_t=["deb"]   :: [string()],
    arch=["all"]        :: [string()],
    maintainers=[]      :: [string()]
}).


%FIXME: move to internal hrl
-record(rev_def, {
    name            :: binary(), 
    branch          :: binary(),
    tag             :: binary(),
    dep_object      :: [version()],
    pkg_config      :: #pkg_config{}
}).


%FIXME: move to internal hrl
-record(build_info, {
    state           :: success|error|none,
    package_spec    :: list(),
    description     :: list(),
    diff            :: list(),
    log             :: list(),
    test_info       :: list()
}).


%FIXME: move to internal hrl
-type version() :: 
    {
        Name        :: binary(),
        Branch      :: binary(),
        Tag         :: binary()
    }.

%% @doc Dependencie record in dependencies dets table. 
%% {PackageVersion, {State, Bucket}, [DependsOn], [HasInDependencies]}
%% - PackageVersion - the version of the current build package;
%% - State - built|error - state of the lates build;
%% - Bucket - Folder identity where the package lays;
%% - DependsOn - Package versions current build depends;
%% - HasInDependencies - Package versions dependent from the current package.
-type dependencie_record() :: {version(), {built|error, [binary()]}, [version()], [version()]}.
