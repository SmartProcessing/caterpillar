-module(caterpillar_deb_plugin).

-export([check/1, prepare/1, submit/1]).

-define(CMD, caterpillar_utils:command).

check(_Dir) ->
    {ok, ""}.

prepare(_Dir) ->
    {ok, ""}.

submit(Dir) ->
    case ?CMD("make package PATH_MOD=../* PATH_MK=../devel-tools/Makefile.mk", Dir) of
        {0, _Msg} ->
            find_deb_file(filename:join([Dir, "dist"]));
        {Code, Msg} when is_integer(Code) ->
            error_logger:info_msg("make package failed with status ~B: ~s", [Code, Msg]),
            {error, io_lib:format("make package returned ~B: ~s", [Code, Msg])}
    end.

find_deb_file(Dir) ->
    case filelib:wildcard(filename:join(Dir, "*.deb")) of
        [Deb] ->
            Name = lists:last(filename:split(Deb)),
            Fd = file:open(Deb, [read]),
            {Fd, Name};
        Other ->
            error_logger:error_msg("cant find deb package, ~p~n", [Other]),
            {error, "couldn't find *.deb package"}
    end.
