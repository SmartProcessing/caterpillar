-define(DEPLOY_DATABASE, "/var/lib/caterpillar/deploy/deploy.db").
-define(DEFAULT_DEPLOY_PATH, "/srv/packages/unknown").

-define(DEPLOY_DB_VERSION, "0.1").

-record(state, {
    ets :: ets:tab(), %{{Type, Branch, Arch}, Path}
    dets :: dets:tab(),
    deploy_path = ?DEFAULT_DEPLOY_PATH,
    deploy_script :: string(),
    registered = false :: boolean(),
    rotate = 5 :: non_neg_integer(), %0 for infinity
    deploy_script_delay = 10 :: pos_integer(),
    deploy_script_timer :: reference(),
    deploy_info = [] :: list(),
    deploy_max_waiting = 2 :: pos_integer()
}).
