-module(caterpillar_build_worker_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("caterpillar.hrl").
-include_lib("caterpillar_internal.hrl").

-define(CBW, caterpillar_build_worker).
-define(CU, caterpillar_utils).

-on_load(tty_off/0).


tty_off() -> 
    error_logger:tty(false).

setup() ->
    dets:open_file('deps', [{file, "deps.dets"}]),
    dets:open_file('buckets', [{file, "buckets.dets"}]),
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
                {<<"caterpillar">>, <<"test">>, <<>>}
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
            {built, ["0001"]},
            [],
            [
                {<<"caterpillar">>, <<"trunk">>, <<>>},
                {<<"pequen">>, <<"trunk">>, <<>>}
            ]
        },
        {
            {<<"caterpillar">>, <<"trunk">>, <<>>},
            {built, ["0001"]},
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
        }
    ],
    filelib:ensure_dir("./test_src/temp/newpkg-trunk1.0.1/"),
    {ok, File} = file:open("./test_src/temp/newpkg-trunk1.0.1/sample", [write]),
    file:write(File, <<"test">>),
    file:close(File),
    filelib:ensure_dir("./test_src/0001/smprc-test"),
    filelib:ensure_dir("./test_src/0001/caterpillar"),
    filelib:ensure_dir("./test_src/0002/smprc-test"),
    filelib:ensure_dir("./test_src/0002/caterpillar"),
    filelib:ensure_dir("./test_src/0003/smprc-test"),
    filelib:ensure_dir("./test_src/0003/caterpillar"),
    dets:insert('deps', DepObj),
    dets:insert('buckets', BucketObj).

cleanup(_Ign) ->
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
            ?CBW:update_buckets('buckets', "./test_src", Rev, [{<<"0001">>, "0001", []}], []),
            ?assertEqual(filelib:is_file("./test_src/0001/newpkg/sample"), true)
        end
    }.
