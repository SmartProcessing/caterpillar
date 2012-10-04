-module(caterpillar_dependencies_tests).
-include_lib("eunit/include/eunit.hrl").
-include("caterpillar.hrl").

-on_load(tty_off/0).


tty_off() -> 
    error_logger:tty(false).

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
                {<<"pequen">>, <<"trunk">>, <<>>}
            ],
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

check_intersection_test() ->
    Rev1 = #rev_def{
        name = <<"caterpillar">>, 
        revision = <<"xf123123gjkdf">>,
        branch = <<"trunk">>,
        tag = <<>>,
        build_id = 3,
        dep_object = [{<<"smprc-test">>, <<"trunk">>, <<>>}]},
    Rev2  =  #rev_def{
        name = <<"new_test">>,
        revision = <<"ajsdasdkads">>,
        branch = <<"trunk">>,
        tag = <<>>,
        build_id = 3,
        dep_object = [{<<"smprc-test">>, <<"trunk">>, <<>>}, {<<"smprc-caterpillar">>, trunk, <<>>}]},
    Rev3  =  #rev_def{
        name = <<"destiny">>,
        revision = <<"ajsD4sdkads">>,
        branch = <<"trunk">>,
        tag = <<>>,
        build_id = 6,
        dep_object = [{<<"pequen">>, <<"trunk">>, <<>>}, {<<"smprc-test">>, trunk, <<>>}]},
    Rev4  =  #rev_def{
        name = <<"depends_on_destiny">>,
        revision = <<"dajfhf987sdf9">>,
        branch = <<"trunk">>,
        tag = <<>>,
        build_id = 6,
        dep_object = [{<<"destiny">>, <<"trunk">>, <<>>}, {<<"smprc-test">>, trunk, <<>>}]},
    ?assertEqual({ok, independent}, caterpillar_dependencies:check_intersection(Rev4, [])),
    ?assertEqual({ok, dependent}, caterpillar_dependencies:check_intersection(Rev4, [Rev3, Rev2, Rev1])).

-endif.
