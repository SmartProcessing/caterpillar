-define(VERSION, caterpillar_utils:get_version_by_revdef).

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

-record(rev_def, {
    name            :: binary(), 
    branch          :: binary(),
    tag             :: binary(),
    dep_object      :: [version()],
    pkg_config      :: #pkg_config{}
}).

-record(build_info, {
    state           :: success|error|none,
    package_spec    :: list(),
    description     :: list(),
    diff            :: list(),
    log             :: list(),
    test_info       :: list()
}).

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
