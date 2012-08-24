-module(caterpillar_api).
-include("caterpillar.hrl").
-export([start/0]).

-ifndef(TEST).
start() ->
    APISet = case application:get_env(caterpillar, api) of
        {ok, Settings} ->
            Settings;
        Other ->
            throw({api_settings_not_set, Other})
    end,
    Port = ?GV(port, APISet, 39567),
    Dispatch = [{'_', [{'_', caterpillar_api_handler, [APISet]}]}],
    logging:info_msg("starting api: ~p",
        [catch cowboy:start_listener(
        caterpillar_api_listener,
        100,
        cowboy_tcp_transport,
        [{port, Port}],
        cowboy_http_protocol,
        [{dispatch, Dispatch}]
    )]).
-endif.



-ifdef(TEST).
start() ->
    ok.
-endif.
