-module(caterpillar_dependencies_tests).
-include_lib("eunit/include/eunit.hrl").
-include("caterpillar.hrl").

-ifdef(TEST).

setup() ->
    dets:open_file('test', [{file, "test.dets"}]),
    DepObj = [
        {
            {<<"smprc-test">>, <<"trunk">>, <<>>},
            built,
            [],
            [
                {<<"caterpillar">>, <<"trunk">>, <<>>},
                {<<"pequen">>, <<"trunk">>, <<>>}
            ]
        },
        {
            {<<"caterpillar">>, <<"trunk">>, <<>>},
            built,
            [{<<"smprc-test">>, <<"trunk">>, <<>>}],
            []
        },
        {
            {<<"pequen">>, <<"trunk">>, <<>>},
            wait,
            [{<<"smprc-test">>, <<"trunk">>, <<>>}],
            [{<<"destiny">>, <<"trunk">>, <<>>}]
        },
        {
            {<<"destiny">>, <<"trunk">>, <<>>},
            wait,
            [
                {<<"smprc-test">>, <<"trunk">>, <<>>},
                {<<"pequen">>, <<"trunk">>, <<>>}],
            []
        }
    ],
    dets:insert('test', DepObj).

cleanup(_Ign) ->
    dets:close('test'),
    os:cmd("rm test.dets").

list_unresolved_dependencies_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun() ->
            ?assertEqual(
                caterpillar_dependencies:list_unresolved_dependencies('test',
                    #rev_def{
                        name = <<"test_package">>,
                        revision = <<"H34114Jr83r9e8J394235">>,
                        branch = <<"trunk">>,
                        tag = <<"">>,
                        build_id = 17,
                        dep_object = [{<<"smprc-test">>, <<"trunk">>, <<"">>},
                        {<<"caterpillar">>, <<"trunk">>, <<"">>}],
                        platform_spec = [<<"default">>]
                    }),
                {ok, []}),
            ?assertEqual(
                caterpillar_dependencies:list_unresolved_dependencies('test',
                    #rev_def{
                        name = <<"test_package">>,
                        revision = <<"H34114Jr83r9e8J394235">>,
                        branch = <<"trunk">>,
                        tag = <<"">>,
                        build_id = 17,
                        dep_object = [{<<"pequen">>, <<"trunk">>, <<"">>}],
                        platform_spec = [<<"default">>]
                    }),
                {ok, [{<<"pequen">>, <<"trunk">>, <<"">>}]})
        end
    }.

-endif.
