-module(caterpillar_notifier_tests).

-include_lib("caterpillar.hrl").
-include_lib("caterpillar_notifier_internal.hrl").
-include_lib("eunit/include/eunit.hrl").






get_name_test() ->
    ?assertEqual(16, length(caterpillar_notifier:get_name())).
