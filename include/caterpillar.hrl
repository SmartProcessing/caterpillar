-define(GV, proplists:get_value).

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


-record(deploy, {
    packages = [] :: [{DeployName :: string(), DeployPackage :: file:io_device()}],
    pre_deploy_actions = [],
    post_deploy_actions = []
}).
