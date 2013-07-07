-ifndef(caterpillar_hrl).
-define(caterpillar_hrl, true).
-define(GV, proplists:get_value).


-record(archive, {
    name            :: string(), %name of package
    branch          :: string(), %branch name
    tag             :: term(), %tags
    archive_name    :: filelib:filename(), %name of archive file
    archive_type    :: zip|tar, %type of archive
    old_revno       :: term(),
    current_revno   :: term(), 
    fd              :: file:io_device() %file descriptor
}).


-record(package, {
    name :: string(),
    branch = '_' :: string()|'_' % '_' - all branches in package
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


-record(ident, {
    arch :: amd64|i386,
    type :: atom()
}).


-record(deploy, {
    ident :: #ident{},
    work_id :: term(),
    tag :: term(),
    packages = [] :: [#deploy_package{}],
    pre_deploy_actions = [],
    post_deploy_actions = [] :: [{module(), function(), [term()]}]
}).


-endif.
