-module(caterpillar_dependencies_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("caterpillar.hrl").
-include_lib("caterpillar_internal.hrl").

-on_load(tty_off/0).


tty_off() -> 
    error_logger:tty(false).

setup() ->
    dets:open_file('test', [{file, "test.dets"}]),
    DepObj = [
        {
            {<<"smprc-test">>, <<"trunk">>, <<>>},
            {built, [<<"0001">>]},
            [],
            [
                {<<"caterpillar">>, <<"trunk">>, <<>>},
                {<<"pequen">>, <<"trunk">>, <<>>}
            ]
        },
        {
            {<<"caterpillar">>, <<"trunk">>, <<>>},
            {built, [<<"0001">>]},
            [{<<"smprc-test">>, <<"trunk">>, <<>>}],
            []
        },
        {
            {<<"pequen">>, <<"trunk">>, <<>>},
            {error, [<<"0001">>]},
            [{<<"smprc-test">>, <<"trunk">>, <<>>}],
            [{<<"destiny">>, <<"trunk">>, <<>>}]
        },
        {
            {<<"destiny">>, <<"trunk">>, <<>>},
            {error, [<<"0001">>]},
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
                        branch = <<"trunk">>,
                        tag = <<"">>,
                        pkg_config={},
                        dep_object = [{<<"smprc-test">>, <<"trunk">>, <<"">>},
                        {<<"caterpillar">>, <<"trunk">>, <<"">>}]
                    }),
                {ok, []}),
            ?assertEqual(
                caterpillar_dependencies:list_unresolved_dependencies('test',
                    #rev_def{
                        name = <<"test_package">>,
                        branch = <<"trunk">>,
                        tag = <<"">>,
                        pkg_config={},
                        dep_object = [{<<"pequen">>, <<"trunk">>, <<"">>}]
                    }),
                {ok, [{<<"pequen">>, <<"trunk">>, <<"">>}]})
        end
    }.

check_intersection_test() ->
    Rev1 = #rev_def{
        name = <<"caterpillar">>, 
        branch = <<"trunk">>,
        tag = <<>>,
        pkg_config={},
        dep_object = [{<<"smprc-test">>, <<"trunk">>, <<>>}]},
    Rev2  =  #rev_def{
        name = <<"new_test">>,
        branch = <<"trunk">>,
        tag = <<>>,
        pkg_config={},
        dep_object = [{<<"smprc-test">>, <<"trunk">>, <<>>}, {<<"smprc-caterpillar">>, <<"trunk">>, <<>>}]},
    Rev3  =  #rev_def{
        name = <<"destiny">>,
        branch = <<"trunk">>,
        tag = <<>>,
        pkg_config={},
        dep_object = [{<<"pequen">>, <<"trunk">>, <<>>}, {<<"smprc-test">>, <<"trunk">>, <<>>}]},
    Rev4  =  #rev_def{
        name = <<"depends_on_destiny">>,
        branch = <<"trunk">>,
        tag = <<>>,
        pkg_config={},
        dep_object = [{<<"destiny">>, <<"trunk">>, <<>>}, {<<"smprc-test">>, <<"trunk">>, <<>>}]},
    ?assertEqual({ok, independent}, caterpillar_dependencies:check_intersection(Rev4, [])),
    ?assertEqual({ok, dependent}, caterpillar_dependencies:check_intersection(Rev4, [Rev3, Rev2, Rev1])).
