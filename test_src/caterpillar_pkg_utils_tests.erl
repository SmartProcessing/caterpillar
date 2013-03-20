-module(caterpillar_pkg_utils_tests).
-include_lib("eunit/include/eunit.hrl").
-include("caterpillar.hrl").
-include("caterpillar_internal.hrl").
-define(CPU, caterpillar_pkg_utils).

-on_load(tty_off/0).


tty_off() -> 
    error_logger:tty(false).

-ifdef(TEST).

parse_control_test() ->
    Sample = 
    [
        {"Package", "caterpillar"},
        {"Section", "smprc"},
        {"Description", "\"package building tools\""},
        {"Architecture", "all"},
        {"Version", "0.0.1"},
        {"Maintainer", "vavdiushkin@smprc.ru"},
        {"Depends", ["erlang-base"]}
    ],
    Res = ?CPU:parse_control("./test_src"),
    lists:map(
        fun({P, V}) ->
            ?assertEqual(?GV(P, Res), V)
        end, Sample).

gen_control_from_pkg_config_test() ->
    Rev = #rev_def{
        name = <<"newpkg">>, 
        branch = <<"trunk">>, 
        tag = <<>>, 
        work_id = 11,
        dep_object = [], 
        pkg_config=#pkg_config{
            name = "newpkg",
            section = "smprc",
            version = "0.0.0",
            arch = "all",
            description = "new package",
            maintainers = ["main@mail.com", "test@mail.com"],
            deps = ["erlang-base", {"testik", "trunk", ""}, "blabla"]
        }
    },
    ?assertEqual(<<"Package: newpkg\nSection: smprc-trunk\nVersion: 0.0.0-trunk.11\nArchitecture: all\nDescription: new package\nMaintainer: main@mail.com\nDepends: blabla, testik, erlang-base\n">>,
        ?CPU:gen_control_from_pkg_config(Rev)).

gen_control_from_pkg_config2_test() ->
    Rev = #rev_def{
        name = <<"cater">>, 
        branch = <<"trunk">>, 
        tag = <<>>, 
        work_id = 11,
        dep_object = [], 
        pkg_config = #pkg_config{
            name= "caterpillar",
            version= "0.14.0",
            section= "smprc",
            package_t= ["deb"],
            arch= "all",
            maintainers= [],
            deps= [
                {"smprc-mochiweb", "trunk", ""},
                {"smprc-cowboy", "trunk", ""}
            ],
            build_deps= [
                {"devel-tools", "trunk", ""},
                {"test_runner", "trunk", ""}] 
        }
    },
    ?debugFmt("~s~n", [?CPU:gen_control_from_pkg_config(Rev)]).


-endif.
