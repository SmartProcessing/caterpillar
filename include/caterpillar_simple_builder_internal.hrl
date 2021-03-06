-ifndef(caterpillar_simple_builder_internal).
-define(caterpillar_simple_builder_internal, true).

-record(state, {
    ident,
    work_id,
    next_work_id,
    work_id_file,
    archive_root,
    repository_root,
    deploy_root
}).


-record(build_package, {
    name :: string(),
    branch :: string(),
    tag :: term(),
    package :: file:name(),
    build_status :: ok | error | ignored,
    log = <<>> :: binary()
}).

-endif.
