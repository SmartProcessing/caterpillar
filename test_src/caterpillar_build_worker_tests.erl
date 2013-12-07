-module(caterpillar_build_worker_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("caterpillar.hrl").
-include_lib("caterpillar_builder_internal.hrl").

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
    ?assertEqual(true, string:equal(Path, "/test/temp/name-trunk-1.0.1.tmp")).
