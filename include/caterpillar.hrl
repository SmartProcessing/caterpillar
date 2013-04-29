-define(GV, proplists:get_value).

-record(archive, {
    name            :: string(),
    branch          :: string(),
    tag             :: term(),
    archive_name    :: filelib:filename(),
    fd              :: file:io_device()
}).


-record(package, {
    name :: string(),
    branch :: string()|'_'
}).


-record(notify, {
    subject = <<>> :: binary,
    body = <<>>    :: binary
}).


-record(deploy_package, {
    name :: string(),
    branch :: string(),
    package :: string(),
    fd :: file:io_device()
}).


-record(deploy, {
    ident :: term(),
    work_id :: term(),
    tag :: term(),
    packages = [] :: [#deploy_package{}],
    pre_deploy_actions = [],
    post_deploy_actions = []
}).
