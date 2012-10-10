-ifndef(caterpillar_notifier_internal).
-define(caterpillar_notifier_internal, true).


-define(MAIL_TEMPLATE,
    "To: ~s~n"
    "From: ~s~n"
    "Subject: ~s~n"
    "~n~n"
    "~s~n"
).
-define(MAIL_ROOT, "/var/lib/caterpillar/notifier/mail_root").


-record(state, {
    mail_root :: filelib:dirname(),
    email_to :: string(),
    email_from :: string()
}).





-endif.
