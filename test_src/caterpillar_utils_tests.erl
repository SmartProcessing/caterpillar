-module(caterpillar_utils_tests).

-on_load(tty_off/0).

-include_lib("caterpillar.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(BUILD_ID, "test_work_id").


tty_off() ->
    error_logger:tty(false).


pipe_test_() ->
{foreach,
    fun() -> ok end,
[
    {Message, fun() ->
        ?assertEqual(
            Result,
            caterpillar_utils:pipe(FunList, PrevRes, State)
        )
    end} || {Message, {FunList, PrevRes, State}, Result} <- [
        {
            "empty funlist", 
            {[], prev_res, state},
            {ok, prev_res}
        },
        {
            "one fun in funlist",
            {[{test, fun(_, _) -> {ok, one} end}], prev_res, state},
            {ok, one}
        },
        {
            "two fun in funlst",
            {[{test, fun(_, _) -> {ok, one} end}, {test, fun(_, _) -> {ok, two} end}], prev_res, state},
            {ok, two}
        },
        {
            "first function return error",
            {[{test, fun(_, _) -> {error, one} end}, {test, fun(_, _) -> {error, two} end}], prev_res, state},
            {error, one}
        },
        {
            "unmatched return",
            {[{test, fun(_, _) -> ok end}], prev_res, state},
            {error, {pipe, ok}}
        }
    ]
]}.


read_work_id_test_() ->
{foreach,
    fun() -> ok end,
    fun(_) -> catch file:delete(?BUILD_ID) end,
[
    {Message, fun() ->
        Setup(),
        ?assertEqual(
            Result,
            caterpillar_utils:read_work_id(?BUILD_ID)
        )
    end} || {Message, Setup, Result} <- [
        {
            "no work id file exists",
            fun() -> ok end,
            1
        },
        {
            "work id file empty",
            fun() -> file:write_file(?BUILD_ID, <<>>) end,
            1
        },
        {
            "work id file got bad data",
            fun() -> file:write_file(?BUILD_ID, <<1,2,3,4>>) end,
            1
        },
        {
            "work id file got correct structure, but not valid data",
            fun() -> file:write_file(?BUILD_ID, <<"{work_id,not_valid}.">>) end,
            1
        },
        {
            "valid work_id file structure", 
            fun() -> file:write_file(?BUILD_ID, <<"{work_id, 2}.">>) end,
            2
        }
    ]
]}.



write_work_id_test_()->
{foreach,
    fun() -> ok end,
    fun(_) -> catch file:delete(?BUILD_ID) end,
[
    {Message, fun() -> 
        ?assertEqual(
            Result,
            caterpillar_utils:write_work_id(?BUILD_ID, BuildId)
        ),
        Check()
    end} || {Message, Check, BuildId, Result} <- [
        {
            "work_id file not exists",
            fun() -> ?assertEqual(caterpillar_utils:read_work_id(?BUILD_ID), 1) end,
            1,
            ok 
        },
        {
            "badarg",
            fun() -> ok end,
            "1",
            {error, badarg}
        }
    ]
]}.


check_work_id_file_test_() ->
{foreach,
    fun() -> catch file:delete(?BUILD_ID) end,
    fun(_) -> ok = file:delete(?BUILD_ID) end,
[
    {Message, fun() -> 
        Setup(),
        ?assertEqual(
            ok,
            caterpillar_utils:check_work_id_file(?BUILD_ID)
        ),
        Check()
    end} || {Message, Setup, Check} <- [
        {
            "file created",
            fun() -> ok end,
            fun() -> ?assert(filelib:is_file(?BUILD_ID)) end
        },
        {
            "file exists",
            fun() -> file:write_file(?BUILD_ID, <<"exists">>) end,
            fun() ->
                 ?assert(filelib:is_file(?BUILD_ID)),
                 ?assertEqual(
                     {ok, <<"exists">>}, file:read_file(?BUILD_ID)
                )
            end
        }
    ]
]}.



package_to_archive_to_package_test_() ->
{foreach,
    fun() -> ok end,
[
    {Message, fun() ->
        ?assertEqual(
            Archive,
            caterpillar_utils:package_to_archive(Package, Branch)
        ),
        ?assertEqual(
            {Package, Branch},
            caterpillar_utils:archive_to_package(Archive)
        )
    end} || {Message, Package, Branch, Archive} <- [
        {
            "valid package n repo",
            "package",
            "branch",
            "package__ARCHIVE__branch"
        }
    ]
]}.



list_packages_test_() ->
{foreachx,
    fun(Setup) -> [caterpillar_utils:ensure_dir(Dir) || Dir <- Setup] end,
    fun(Setup, _) ->  [file:del_dir(Dir) || Dir <- Setup] end,
[
    {Setup, fun(_, _) ->
        {Message, fun() ->
            ?assertEqual(Result, caterpillar_utils:list_packages(Path))
        end}
    end} || {Message, Setup, Path, Result} <- [
        {
            "list packages in existing dir",
            ["__test/package1/", "__test/package2/", "__test/package3/", "__test/абцд", "__test"],
            "__test",
            {ok, ["package1", "package2", "package3", "абцд"]}
        },
        {
            "bad path, dir not exists",
            [],
            "__test",
            {error, enoent}
        }
    ]
]}.


del_dir_test_() ->
{foreachx,
    fun(Setup) -> [filelib:ensure_dir(Dir) || Dir <- Setup] end,
    fun(_, _) ->  ok end,
[
    {Setup, fun(Setup_, _) ->
        {Message, fun() ->
            ?assertEqual(
                ok,
                caterpillar_utils:del_dir(Package)
            ),
            [?assert(not filelib:is_dir(Dir)) || Dir <- Setup_]
        end}
    end} || {Message, Setup, Package, Result} <- [
        {
            "delete nested directories",
            ["__test/package1/", "__test/package2/", "__test/package3/", "__test"],
            "__test",
            ok
        }
    ]
]}.


ensure_dir_test_() ->
{foreachx,
    fun(Dir) -> caterpillar_utils:del_dir(Dir) end,
    fun(Dir, _) -> caterpillar_utils:del_dir(Dir) end,
[
    {Dir, fun(_, _) ->
        {Message, fun() ->
            ?assert(not filelib:is_dir(Dir)),
            caterpillar_utils:ensure_dir(Dir),
            ?assert(filelib:is_dir(Dir)),
            ?assertEqual(caterpillar_utils:list_packages(Dir), {ok, []})
        end}
    end} || {Message, Dir} <- [
        {"dir passed without '/' in the end", "ensure_dir1"},
        {"dir passed with '/' in the end", "ensure_dir/"},
        {"dir passed with few '/' in the end", "ensure_dir////"}
    ]
]}.



get_value_or_die_test_() ->
{foreach,
    fun() -> ok end,
[
    {Message, fun() ->
        ?assertEqual(
            Result,
            catch caterpillar_utils:get_value_or_die(Key, Data)
        )
    end} || {Message, Key, Data, Result} <- [
        {
            "key not found",
            test,
            [],
            {'EXIT', {no_value, test}}
        },
        {
            "key found",
            key,
            [{key, value}],
            value
        }
        
    ]
]}.


filename_join_test_() ->
{foreach, fun() -> ok end,
[
    {Message, fun() ->
        ?assertEqual(Result, caterpillar_utils:filename_join(Data))
    end} || {Message, Data, Result} <- [
        {
            "empty list",
            [],
            []
        },
        {
            "one element in the list(string)",
            ["test"],
            "test"
        },
        {
            "one element in the list(binary)",
            [<<"test">>],
            "test"
        },
        {
            "few elements in the list(string and binary)",
            ["t1", <<"t2">>],
            "t1/t2"
        },
        {
            "few elements in the list(only strings with unicode symbols)",
            ["t1", "абцд"],
            "t1/абцд"
        },
        {
            "few elements in the list(string and list with unicode symbols)",
            ["t1", "т1", <<"t2">>, <<208, 176, 208, 176, "2">>],
            "t1/т1/t2/аа2"
        },
        {
            "few elements, latin1",
            ["t1", "t2", "t3", "t4", "t5"],
            "t1/t2/t3/t4/t5"
        }
    ]
]}.


gen_ident_test_() ->
{foreach, fun() -> ok end,
[
    {Message, fun() ->
        ?assertEqual(Result, caterpillar_utils:gen_ident(Type, Arch))
    end} || {Message, Type, Arch, Result} <- [
        {"from atom/list", squeeze, "amd64", #ident{type= <<"squeeze">>, arch= <<"amd64">>}},
        {"from binary/integer", <<"bin">>, 2141, #ident{type= <<"bin">>, arch= <<"2141">>}},
        {"from tuple/float", {'wat?'}, 1.125, #ident{type= <<"{'wat?'}">>, arch= <<"1.125">>}}
    ]
]}.


any_ident_test() ->
    ?assertEqual('_', caterpillar_utils:any_ident()).


do_in_pool_test_() ->
{foreach, fun() -> ok end,
[
    {
        "testing parralel work",
        fun() -> 
            Start = caterpillar_utils:unixtime() / 1000,
            Cb = fun(_) -> timer:sleep(100) end,
            caterpillar_utils:do_in_pool(Cb, 5, [1,1,1,1,1,1]),
            End = caterpillar_utils:unixtime() / 1000,
            ?assert(200 < End - Start andalso End - Start < 250)
        end
    },
    {
        "testing sequential work",
        fun() ->
            Start = caterpillar_utils:unixtime() / 1000,
            Cb = fun(_) -> timer:sleep(10) end,
            caterpillar_utils:do_in_pool(Cb, 1, lists:seq(1, 10)),
            End = caterpillar_utils:unixtime() / 1000,
            ?assert(100 < End - Start andalso End - Start < 150)
        end
    },
    {
        "cb crashed",
        fun() ->
            Cb = fun(_) -> exit(some_reason) end,
            ?assertEqual([{'EXIT', some_reason}], caterpillar_utils:do_in_pool(Cb, 5, [1]))
        end
    }
]}.
