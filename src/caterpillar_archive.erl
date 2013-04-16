-module(caterpillar_archive).


-export([export/2]).



-spec export(Archive::filelib:filename(), Options::proplists:property()) -> ok|{error, Reason::term()}.
export(Archive, Options) ->
    Cwd = caterpillar_utils:get_value_or_die(cwd, Options),
    Type = caterpillar_utils:get_value_or_die(type, Options),
    Command = get_command_by_type(Type, Archive),
    case caterpillar_utils:command(Command, [{cwd, Cwd}]) of
        {0, _} -> ok;
        {Code, Data} -> 
            {error, {export, {Code, Data}}};
        Other ->
            {error, {export, {bad_response, Other}}}
    end.



get_command_by_type(tgz, Archive) -> lists:flatten(io_lib:format("tar -xzf ~s", [Archive]));
get_command_by_type(tar, Archive) -> lists:flatten(io_lib:format("tar -xf ~s", [Archive])).
