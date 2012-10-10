#!/usr/bin/env escript


check_value("self") ->
    self;
check_value("cookie") ->
    cookie;
check_value(Other) ->
    io:format("bad cmd ~s~n", [Other]),
    halt(1).



return_value(Services, Cmd) ->
    F = fun({caterpillar_net_kernel, _, _}) -> true;(_) -> false end,
    case lists:filter(F, Services) of
        [] ->
            io:format(
                "bad config, no section for caterpillar_net_kernel~n"
            ),
            halt(1);
        [{_, _, Opts}|_] ->
            io:format("~s~n", [proplists:get_value(Cmd, Opts)])
    end.



main([Config, Cmd]) ->
    ConfigFile = lists:last(filename:split(Config)),
    [SectionStr|_] = string:tokens(ConfigFile, "."),
    Section = list_to_atom(SectionStr),
    AtomCmd = check_value(Cmd),
    {ok, [File]} = file:consult(Config),
    BM = proplists:get_value(Section, File),
    SS = proplists:get_value(services, BM),
    return_value(SS, AtomCmd);
main(_) ->
    io:format("usage: caterpillar.escript config_path self|cookie").


