-define(DEPLOY_DATABASE, "/var/lib/caterpillar/deploy/deploy.db").

-record(state, {
    ets :: ets:tab(),
    dets :: dets:tab(),
    deploy_script :: string(),
    registered=false :: boolean(),
    rotate = 5 :: non_neg_integer() %0 for infinity
}).
