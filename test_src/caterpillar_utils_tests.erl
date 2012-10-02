-module(caterpillar_utils_tests).

-on_load(tty_off/0).

-include_lib("eunit/include/eunit.hrl").
-define(BUILD_ID, "test_build_id").


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


read_build_id_test_() ->
{foreach,
    fun() -> ok end,
    fun(_) -> catch file:delete(?BUILD_ID) end,
[
    {Message, fun() ->
        Setup(),
        ?assertEqual(
            Result,
            caterpillar_utils:read_build_id(?BUILD_ID)
        )
    end} || {Message, Setup, Result} <- [
        {
            "no build id file exists",
            fun() -> ok end,
            1
        },
        {
            "build id file empty",
            fun() -> file:write_file(?BUILD_ID, <<>>) end,
            1
        },
        {
            "build id file got bad data",
            fun() -> file:write_file(?BUILD_ID, <<1,2,3,4>>) end,
            1
        },
        {
            "build id file got correct structure, but not valid data",
            fun() -> file:write_file(?BUILD_ID, <<"{build_id,not_valid}.">>) end,
            1
        },
        {
            "valid build_id file structure", 
            fun() -> file:write_file(?BUILD_ID, <<"{build_id, 2}.">>) end,
            2
        }
    ]
]}.



write_build_id_test_()->
{foreach,
    fun() -> ok end,
    fun(_) -> catch file:delete(?BUILD_ID) end,
[
    {Message, fun() -> 
        ?assertEqual(
            Result,
            caterpillar_utils:write_build_id(?BUILD_ID, BuildId)
        ),
        Check()
    end} || {Message, Check, BuildId, Result} <- [
        {
            "build_id file not exists",
            fun() -> ?assertEqual(caterpillar_utils:read_build_id(?BUILD_ID), 1) end,
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


check_build_id_file_test_() ->
{foreach,
    fun() -> catch file:delete(?BUILD_ID) end,
    fun(_) -> ok = file:delete(?BUILD_ID) end,
[
    {Message, fun() -> 
        Setup(),
        ?assertEqual(
            ok,
            caterpillar_utils:check_build_id_file(?BUILD_ID)
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
