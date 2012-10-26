-module(caterpillar_simple_builder_tests).

-include_lib("caterpillar_simple_builder_internal.hrl").
-include_lib("eunit/include/eunit.hrl").

-on_load(tty_off/0).
-define(TEST_CONTROL, "__test_control").

-define(
    TEST_CONTROL_DATA,
    "Package: caterpillar\nSection: smprc\nDescription: \"package building tools\"\n"
    "Architecture: ~s\nVersion: 0.2.1\nMaintainer: vavdiushkin@smprc.ru\n"
    "Depends: erlang-base(>=14)\n"
).

tty_off() ->
    error_logger:tty(false).


tty_on() ->
    error_logger:tty(true).


modity_contol_test_() ->
{foreachx,
    fun(ControlData) ->
        tty_on(),
        file:write_file(?TEST_CONTROL, ControlData)
    end,
    fun(_, _) ->
        tty_off(),
        file:delete(?TEST_CONTROL)
    end,
[
    {ControlData, fun(_, _) ->
        {Message, fun() ->
            caterpillar_simple_builder:modify_control(?TEST_CONTROL, "branch", 1, Ident),
            ?assertEqual(
                {ok, Result},
                file:read_file(?TEST_CONTROL)
            )
        end}
    end} || {Message, ControlData, Ident, Result} <- [
        {
            "all architecture",
            list_to_binary(io_lib:format(?TEST_CONTROL_DATA, ["all"])),
            amd64,
            <<
              "Package: caterpillar\nSection: smprc-branch\nDescription: \"package building tools\"\n"
              "Architecture: all\nVersion: 0.2.1-branch.1\nMaintainer: vavdiushkin@smprc.ru\n"
              "Depends: erlang-base(>=14)\n\n"
            >>
        },
        {
            "ident-specific architecture(amd64)",
            list_to_binary(io_lib:format(?TEST_CONTROL_DATA, [""])),
            amd64,
            <<
              "Package: caterpillar\nSection: smprc-branch\nDescription: \"package building tools\"\n"
              "Architecture: amd64\nVersion: 0.2.1-branch.1\nMaintainer: vavdiushkin@smprc.ru\n"
              "Depends: erlang-base(>=14)\n\n"
            >>
        },
        {
            "ident-specific architecture(i386)",
            list_to_binary(io_lib:format(?TEST_CONTROL_DATA, [""])),
            i386,
            <<
              "Package: caterpillar\nSection: smprc-branch\nDescription: \"package building tools\"\n"
              "Architecture: i386\nVersion: 0.2.1-branch.1\nMaintainer: vavdiushkin@smprc.ru\n"
              "Depends: erlang-base(>=14)\n\n"
            >>
        }
    ]
]}.
