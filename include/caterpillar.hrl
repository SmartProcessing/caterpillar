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
