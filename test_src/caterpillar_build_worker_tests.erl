-module(caterpillar_build_worker_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("caterpillar.hrl").
-include_lib("caterpillar_internal.hrl").

-define(CBW, caterpillar_build_worker).
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
            {error, []},
            [{<<"smprc-test">>, <<"trunk">>, <<>>}],
            [{<<"destiny">>, <<"trunk">>, <<>>}]
        },
        {
            {<<"destiny">>, <<"trunk">>, <<>>},
            {error, []},
            [
                {<<"smprc-test">>, <<"trunk">>, <<>>},
                {<<"pequen">>, <<"trunk">>, <<>>}
            ],
            []
        },
        {
            {<<"newpkg">>, <<"trunk">>, <<"1.0.1">>},
            {in_process, [<<"0003">>]},
            [
                {<<"smprc-test">>, <<"trunk">>, <<>>},
                {<<"caterpillar">>, <<"trunk">>, <<>>}
            ],
            []
        },
        {
            {<<"newpkg_dep">>, <<"test">>, <<>>},
            {built, [<<"0002">>]},
            [],
            [{<<"newpkg">>, <<"trunk">>, <<"1.0.1">>}]
        }
    ],
    filelib:ensure_dir("./test_src/temp/newpkg-trunk1.0.1/"),
    {ok, File} = file:open("./test_src/temp/newpkg-trunk1.0.1/sample", [write]),
    file:write(File, <<"{test, 1}.">>),
    file:close(File),
    filelib:ensure_dir("./test_src/0002/newpkg_dep/"),
    {ok, ArmFile} = file:open("./test_src/0002/newpkg_dep/dep_test", [write]),
    file:write(ArmFile, <<"{test, 2}.">>),
    file:close(ArmFile),
    filelib:ensure_dir("./test_src/0001/smprc-test"),
    filelib:ensure_dir("./test_src/0001/caterpillar"),
    filelib:ensure_dir("./test_src/0002/smprc-test"),
    filelib:ensure_dir("./test_src/0002/caterpillar"),
    filelib:ensure_dir("./test_src/0003/smprc-test"),
    filelib:ensure_dir("./test_src/0003/caterpillar"),
    dets:insert('deps', DepObj),
    dets:insert('buckets', BucketObj).

cleanup(_Ign) ->
    gen_server:call(caterpillar_lock, stop),
    dets:close('buckets'),
    dets:close('deps'),
    ?CU:del_dir("./test_src/temp"),
    ?CU:del_dir("./test_src/0001"),
    ?CU:del_dir("./test_src/0002"),
    ?CU:del_dir("./test_src/0003"),
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
    ?assertEqual(?CBW:validate_bucket([{<<"1">>, <<"1">>, <<"1">>}], []), true),
    ?assertEqual(?CBW:validate_bucket([{<<"1">>, <<"1">>, <<"1">>}], [{<<"1">>, <<"1">>, <<"1">>}]), true),
    ?assertEqual(?CBW:validate_bucket([{<<"1">>, <<"1">>, <<"1">>}], [{<<"1">>, <<"2">>, <<"1">>}]), false),
    ?assertEqual(?CBW:validate_bucket([{<<"1">>, <<"1">>, <<"1">>}, {<<"2">>, <<"2">>, <<"2">>}], [{<<"1">>, <<"1">>, <<"1">>}]), true),
    ?assertEqual(?CBW:validate_bucket([{<<"1">>, <<"1">>, <<"1">>}, {<<"2">>, <<"4">>, <<"2">>}], [{<<"1">>, <<"1">>, <<"1">>}, {<<"2">>, <<"2">>, <<"2">>}]), false).

find_bucket_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun() ->
            Bucket1 = dets:lookup('buckets', <<"0001">>),
            Bucket2 = dets:lookup('buckets', <<"0002">>),
            Bucket3 = dets:lookup('buckets', <<"0003">>),
            ?assertEqual(?CBW:find_bucket('buckets', {<<"perceptron">>, <<"trunk">>, <<"1.0.1">>}, [{<<"caterpillar">>, <<"trunk">>, <<>>}]), Bucket1),
            ?assertEqual(?CBW:find_bucket('buckets', {<<"perceptron">>, <<"trunk">>, <<"1.0.1">>}, [{<<"smprc-test">>, <<"stable">>, <<"1.2">>}]), Bucket3),
            ?assertEqual(?CBW:find_bucket('buckets', {<<"caterpillar">>, <<"test">>, <<>>}, []), Bucket2),
            ?assertMatch([_Buck], ?CBW:find_bucket('buckets', {<<"perceptron">>, <<"trunk">>, <<"1.0.1">>}, [])),
            ?assertEqual(?CBW:find_bucket('buckets', {<<"perceptron">>, <<"trunk">>, <<"1.0.1">>}, 
                    [
                        {<<"smprc-test">>, <<"trunk">>, <<>>},
                        {<<"caterpillar">>, <<"trunk">>, <<>>}
                    ])
                    , Bucket1)
        end
    }.

get_temp_path_test() ->
    Path = ?CBW:get_temp_path("/test", 
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
            ?CBW:update_buckets('buckets', "./test_src", "./test_src/temp/newpkg-trunk1.0.1", Rev, [
                    {<<"0001">>, "0001", []}, 
                    {<<"0002">>, "0002", []}, 
                    {<<"0003">>, "0003", []}], []),
            ?assertEqual(file:consult("./test_src/0001/newpkg/sample"), {ok, [{test, 1}]}),
            ?assertEqual(file:consult("./test_src/0002/newpkg/sample"), {ok, [{test, 1}]}),
            ?assertEqual(file:consult("./test_src/0003/newpkg/sample"), {ok, [{test, 1}]})
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
            ?CBW:update_package_buckets('buckets', 'deps', [{<<"0001">>, "0001", []}], "./test_src", "./test_src/temp/newpkg-trunk1.0.1",  Rev),
            ?assertEqual(file:consult("./test_src/0001/newpkg/sample"), {ok, [{test, 1}]}),
            ResB = dets:lookup('buckets', <<"0001">>),
            ?assertEqual([{<<"0001">>, "0001", [{<<"newpkg">>, <<"trunk">>, <<"1.0.1">>}]}], ResB),
            [ResD] = dets:lookup('deps', {<<"newpkg">>, <<"trunk">>, <<"1.0.1">>}),
            ?assertMatch({_, {in_process, [<<"0001">>, <<"0003">>]}, _, _}, ResD)
        end
    }.

arm_build_bucket_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun() ->
            Vsn = {<<"newpkg_dep">>, <<"test">>, <<>>},
            ?CBW:arm_build_bucket('buckets', 'deps', {<<"0001">>, "0001", []}, "./test_src", [Vsn]),
            ?assertEqual(file:consult("./test_src/0001/newpkg_dep/dep_test"), {ok, [{test, 2}]})
        end
    }.
