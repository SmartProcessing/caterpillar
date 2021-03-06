-module(caterpillar_build_storage_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("caterpillar.hrl").
-include_lib("caterpillar_builder_internal.hrl").

-on_load(tty_off/0).


tty_off() -> 
    error_logger:tty(false).

setup() ->
    dets:open_file('test', [{file, "test.dets"}]),
    catch caterpillar_lock_sup:start_link(),
    DepObj = [
        {
            {<<"smprc-test">>, <<"trunk">>, <<>>},
            <<"built">>,
            [],
            [
                {<<"caterpillar">>, <<"trunk">>, <<>>},
                {<<"pequen">>, <<"trunk">>, <<>>}
            ]
        },
        {
            {<<"caterpillar">>, <<"trunk">>, <<>>},
            <<"built">>,
            [{<<"smprc-test">>, <<"trunk">>, <<>>}],
            []
        },
        {
            {<<"pequen">>, <<"trunk">>, <<>>},
            <<"none">>,
            [{<<"smprc-test">>, <<"trunk">>, <<>>}],
            [{<<"destiny">>, <<"trunk">>, <<>>}]
        },
        {
            {<<"destiny">>, <<"trunk">>, <<>>},
            <<"none">>,
            [
                {<<"smprc-test">>, <<"trunk">>, <<>>},
                {<<"pequen">>, <<"trunk">>, <<>>}
            ],
            []
        }
    ],
    dets:insert('test', DepObj).

cleanup(_Ign) ->
    gen_server:call(caterpillar_lock, stop),
    dets:close('test'),
    os:cmd("rm test.dets").

list_unresolved_dependencies_test_() ->
   {setup,
       fun setup/0,
       fun cleanup/1,
       fun() ->
           ?assertEqual(
               caterpillar_build_storage:list_unres_deps('test',
                   #rev_def{
                       name = <<"test_package">>,
                       branch = <<"trunk">>,
                       tag = <<"">>,
                       pkg_config={},
                       dep_object = [{{<<"smprc-test">>, <<"trunk">>, <<"">>}, <<"built">>},
                           {{<<"caterpillar">>, <<"trunk">>, <<"">>}, <<"built">>}]
                   },
                   []
               ),
               {ok, [], []}),
           ?assertEqual(
               caterpillar_build_storage:list_unres_deps('test',
                   #rev_def{
                       name = <<"test_package">>,
                       branch = <<"trunk">>,
                       tag = <<"">>,
                       pkg_config={},
                       dep_object = [{{<<"pequen">>, <<"trunk">>, <<"">>}, <<"built">>}]
                   },
                   []
               ),
               {ok, [{<<"pequen">>, <<"trunk">>, <<"">>}], []})
       end
   }.

check_isect_test() ->
    Rev1 = #rev_def{
        name = <<"caterpillar">>, 
        branch = <<"trunk">>,
        tag = <<>>,
        pkg_config={},
        dep_object = [{{<<"smprc-test">>, <<"trunk">>, <<>>}, <<"built">>}]},
    Rev2  =  #rev_def{
        name = <<"new_test">>,
        branch = <<"trunk">>,
        tag = <<>>,
        pkg_config={},
        dep_object = [{{<<"smprc-test">>, <<"trunk">>, <<>>}, <<"built">>}, {{<<"smprc-caterpillar">>, <<"trunk">>, <<>>}, <<"built">>}]},
    Rev3  =  #rev_def{
        name = <<"destiny">>,
        branch = <<"trunk">>,
        tag = <<>>,
        pkg_config={},
        dep_object = [{{<<"pequen">>, <<"trunk">>, <<>>}, <<"built">>}, {{<<"smprc-test">>, <<"trunk">>, <<>>}, <<"built">>}]},
    Rev4  =  #rev_def{
        name = <<"depends_on_destiny">>,
        branch = <<"trunk">>,
        tag = <<>>,
        pkg_config={},
        dep_object = [{{<<"destiny">>, <<"trunk">>, <<>>}, <<"built">>}, {{<<"smprc-test">>, <<"trunk">>, <<>>}, <<"built">>}]},
    ?assertEqual({ok, independent}, caterpillar_build_storage:check_isect(Rev4, [])),
    ?assertEqual({ok, dependent}, caterpillar_build_storage:check_isect(Rev4, [Rev3, Rev2, Rev1])).
