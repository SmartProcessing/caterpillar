#!/usr/bin/env escript


check_value("self") ->
    self;
check_value("cookie") ->
    cookie;
check_value(Other) ->
    io:format("bad cmd ~s~n", [Other]),
    halt(1).



return_value(Section, Cmd) ->
    case proplists:get_value(Cmd, Section, '$undefined$') of
        '$undefined$' -> io:format("~p not found in ~p~n", [Cmd, Section]);
        Value -> io:format("~p~n", [Value])
    end.


main([Config, Cmd]) ->
    case file:consult(Config) of
        {ok, [[{caterpillar, Data}]]} ->
            return_value(
                proplists:get_value(net_kernel, Data),
                check_value(Cmd)
            );
        Err -> 
            io:format("bad consult result ~p~n", [Err]),
            halt(1)
    end;
main(_) ->
    io:format("usage: caterpillar.escript config_path self|cookie").


