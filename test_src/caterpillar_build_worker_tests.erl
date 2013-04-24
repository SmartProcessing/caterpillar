-module(caterpillar_build_worker_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("caterpillar.hrl").
-include_lib("caterpillar_internal.hrl").

-define(CBS, caterpillar_build_storage).
-define(CU, caterpillar_utils).

-on_load(tty_off/0).


tty_on() ->
    error_logger:tty(true).


tty_off() -> 
    error_logger:tty(false).


setup() ->
    dets:open_file('deps', [{file, "deps.dets"}]),
    dets:open_file('buckets', [{file, "buckets.dets"}]),
    catch caterpillar_lock_sup:start_link(),
    BucketObj = [
        {<<"0001">>, "0001", 
            [
                {<<"smprc-test">>, <<"trunk">>, <<>>},
                {<<"caterpillar">>, <<"trunk">>, <<>>}
            ]
        },
        {<<"0002">>, "0002", 
            [
                {<<"smprc-test">>, <<"test">>, <<>>},
                {<<"caterpillar">>, <<"test">>, <<>>},
                {<<"newpkg_dep">>, <<"test">>, <<>>}
            ]
        },
        {<<"0003">>, "0003", 
            [
                {<<"smprc-test">>, <<"stable">>, <<"1.2">>},
                {<<"caterpillar">>, <<"stable">>, <<>>}
            ]
        }
    ],
    DepObj = [
        {
            {<<"smprc-test">>, <<"trunk">>, <<>>},
            {<<"built">>, [<<"0001">>]},
            [],
            [
                {<<"caterpillar">>, <<"trunk">>, <<>>},
                {<<"pequen">>, <<"trunk">>, <<>>}
            ]
        },
        {
            {<<"caterpillar">>, <<"trunk">>, <<>>},
            {<<"built">>, [<<"0001">>]},
            [{<<"smprc-test">>, <<"trunk">>, <<>>}],
            []
        },
        {
            {<<"pequen">>, <<"trunk">>, <<>>},
            {<<"error">>, []},
            [{<<"smprc-test">>, <<"trunk">>, <<>>}],
            [{<<"destiny">>, <<"trunk">>, <<>>}]
        },
        {
            {<<"destiny">>, <<"trunk">>, <<>>},
            {<<"error">>, []},
            [
                {<<"smprc-test">>, <<"trunk">>, <<>>},
                {<<"pequen">>, <<"trunk">>, <<>>}
            ],
            []
        },
        {
            {<<"newpkg">>, <<"trunk">>, <<"1.0.1">>},
            {<<"new">>, [<<"0003">>]},
            [
                {<<"smprc-test">>, <<"trunk">>, <<>>},
                {<<"caterpillar">>, <<"trunk">>, <<>>}
            ],
            []
        },
        {
            {<<"newpkg_dep">>, <<"test">>, <<>>},
            {<<"built">>, [<<"0002">>]},
            [],
            [{<<"newpkg">>, <<"trunk">>, <<"1.0.1">>}]
        }
    ],
    %FIXME: why use temp under inside test dir?
    filelib:ensure_dir("./test/temp/newpkg-trunk1.0.1/"),
    {ok, File} = file:open("./test/temp/newpkg-trunk1.0.1/sample", [write]),
    file:write(File, <<"{test, 1}.">>),
    file:close(File),
    filelib:ensure_dir("./test/0002/newpkg_dep/"),
    {ok, ArmFile} = file:open("./test/0002/newpkg_dep/dep_test", [write]),
    file:write(ArmFile, <<"{test, 2}.">>),
    file:close(ArmFile),
    filelib:ensure_dir("./test/0001/smprc-test"),
    filelib:ensure_dir("./test/0001/caterpillar"),
    filelib:ensure_dir("./test/0002/smprc-test"),
    filelib:ensure_dir("./test/0002/caterpillar"),
    filelib:ensure_dir("./test/0003/smprc-test"),
    filelib:ensure_dir("./test/0003/caterpillar"),
    dets:insert('deps', DepObj),
    dets:insert('buckets', BucketObj).

cleanup(_Ign) ->
    gen_server:call(caterpillar_lock, stop),
    dets:close('buckets'),
    dets:close('deps'),
    ?CU:del_dir("./test/temp"),
    ?CU:del_dir("./test/0001"),
    ?CU:del_dir("./test/0002"),
    ?CU:del_dir("./test/0003"),
    os:cmd("rm deps.dets"),
    os:cmd("rm buckets.dets").

list_unresolved_dependencies_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun() ->
            pass
        end
    }.

validate_bucket_test() ->
    ?assertEqual(?CBS:validate_bucket([{<<"1">>, <<"1">>, <<"1">>}], []), true),
    ?assertEqual(?CBS:validate_bucket([{<<"1">>, <<"1">>, <<"1">>}], [{<<"1">>, <<"1">>, <<"1">>}]), true),
    ?assertEqual(?CBS:validate_bucket([{<<"1">>, <<"1">>, <<"1">>}], [{<<"1">>, <<"2">>, <<"1">>}]), false),
    ?assertEqual(?CBS:validate_bucket([{<<"1">>, <<"1">>, <<"1">>}], [{<<"1">>, <<"1">>, <<"2">>}]), false),
    ?assertEqual(?CBS:validate_bucket([{<<"1">>, <<"1">>, <<"1">>}, {<<"2">>, <<"2">>, <<"2">>}], [{<<"1">>, <<"1">>, <<"1">>}]), true),
    ?assertEqual(?CBS:validate_bucket([{<<"1">>, <<"1">>, <<"1">>}, {<<"2">>, <<"4">>, <<"2">>}], [{<<"1">>, <<"1">>, <<"1">>}, {<<"2">>, <<"2">>, <<"2">>}]), false).

find_bucket1_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun() ->
            Bucket1 = dets:lookup('buckets', <<"0001">>),
            ?assertEqual(?CBS:find_bucket('buckets', {<<"perceptron">>, <<"trunk">>, <<"1.0.1">>}, [{<<"caterpillar">>, <<"trunk">>, <<>>}]), Bucket1)
        end
    }.

find_bucket2_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun() ->
            Bucket2 = dets:lookup('buckets', <<"0002">>),
            ?assertEqual(?CBS:find_bucket('buckets', {<<"caterpillar">>, <<"test">>, <<>>}, []), Bucket2)
        end
    }.

find_bucket3_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun() ->
            Bucket3 = dets:lookup('buckets', <<"0003">>),
            ?assertEqual(?CBS:find_bucket('buckets', {<<"perceptron">>, <<"trunk">>, <<"1.0.1">>}, [{<<"smprc-test">>, <<"stable">>, <<"1.2">>}]), Bucket3)
        end
    }.

find_bucket4_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun() ->
            Bucket1 = dets:lookup('buckets', <<"0001">>),
            ?assertEqual(?CBS:find_bucket('buckets', {<<"perceptron">>, <<"trunk">>, <<"1.0.1">>}, 
                    [
                        {<<"smprc-test">>, <<"trunk">>, <<>>},
                        {<<"caterpillar">>, <<"trunk">>, <<>>}
                    ])
                    , Bucket1)
        end
    }.

find_bucket5_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun() ->
            ?assertMatch([_Buck], ?CBS:find_bucket('buckets', {<<"perceptron">>, <<"trunk">>, <<"1.0.1">>}, []))
        end
    }.

get_temp_path_test() ->
    Path = ?CBS:get_temp_path("/test", 
            #rev_def{
                name = <<"name">>, 
                branch = <<"trunk">>, 
                tag = <<"1.0.1">>, 
                dep_object = [], 
                pkg_config=#pkg_config{name = "name"}
            }
    ),
    ?assertEqual(true, string:equal(Path, "/test/temp/name-trunk1.0.1")).

update_bucket_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun() ->
            Rev = #rev_def{
                name = <<"newpkg">>, 
                branch = <<"trunk">>, 
                tag = <<"1.0.1">>, 
                dep_object = [], 
                pkg_config=#pkg_config{name = "name"}
            },
            ?CBS:update_buckets('buckets', "./test", "./test/temp/newpkg-trunk1.0.1", Rev, [
                    {<<"0001">>, "0001", []}, 
                    {<<"0002">>, "0002", []}, 
                    {<<"0003">>, "0003", []}], []),
            ?assertEqual(file:consult("./test/0001/newpkg/sample"), {ok, [{test, 1}]}),
            ?assertEqual(file:consult("./test/0002/newpkg/sample"), {ok, [{test, 1}]}),
            ?assertEqual(file:consult("./test/0003/newpkg/sample"), {ok, [{test, 1}]})
        end
    }.

update_package_buckets_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun() ->
            Rev = #rev_def{
                name = <<"newpkg">>, 
                branch = <<"trunk">>, 
                tag = <<"1.0.1">>, 
                dep_object = [], 
                pkg_config=#pkg_config{name = "name"}
            },
            ?CBS:update_dep_buckets('buckets', 'deps', [{<<"0001">>, "0001", []}], "./test", "./test/temp/newpkg-trunk1.0.1",  Rev),
            ?assertEqual(file:consult("./test/0001/newpkg/sample"), {ok, [{test, 1}]}),
            [{BName, BPath, BContain}] = dets:lookup('buckets', <<"0001">>),
            ?assertEqual(<<"0001">>, BName),
            ?assertEqual(lists:member({<<"newpkg">>, <<"trunk">>, <<"1.0.1">>}, BContain), true),
            [ResD] = dets:lookup('deps', {<<"newpkg">>, <<"trunk">>, <<"1.0.1">>}),
            ?assertMatch({_, {<<"new">>, [<<"0001">>, <<"0003">>]}, _, _}, ResD)
        end
    }.

arm_build_bucket_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun() ->
            Vsn = {<<"newpkg_dep">>, <<"test">>, <<>>},
            ?CBS:arm_bucket('buckets', 'deps', {<<"0001">>, "0001", []}, "./test", [Vsn]),
            ?assertEqual(file:consult("./test/0001/newpkg_dep/dep_test"), {ok, [{test, 2}]})
        end
    }.
