-define(GV, proplists:get_value).

-record(archive, {
    name            :: string(), %name of package
    branch          :: string(), %branch name
    tag             :: term(), %tags
    archive_name    :: filelib:filename(), %name of archive file
    archive_type    :: zip|tar, %type of archive
    fd              :: file:io_device() %file descriptor
}).


-record(notify, {
    subject = <<>> :: binary(),
    body = <<>>    :: binary()
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
