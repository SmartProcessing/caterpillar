-ifndef(caterpillar_notifier_internal).
-define(caterpillar_notifier_internal, true).


-define(MAIL_TEMPLATE,
    "To: ~s~n"
    "From: ~s~n"
    "Subject: ~s~n"
    "Mime-Version: 1.0~n"
    "Content-type: text/plain; charset=\"utf-8\"~n"
    "~s~n"
).
-define(MAIL_ROOT, "/var/lib/caterpillar/notifier/mail_root").


-record(state, {
    ets :: ets:tab(),
    email_backend :: module(),
    registered = false :: boolean(),
    mail_root :: filelib:dirname(),
    email_to :: string(),
    email_from :: string(),
    email_distribution :: list()
}).





-endif.
