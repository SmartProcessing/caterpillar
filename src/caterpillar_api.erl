-module(caterpillar_api).
-include("caterpillar.hrl").
-export([start/1]).

start(Settings) ->
    APISet = ?GV(api_settings, Settings, []),
    Port = ?GV(port, Settings, 39567),
    Dispatch = [{'_', [{'_', caterpillar_api_handler, [APISet]}]}],
    cowboy:start_listener(
        caterpillar_api_listener,
        100,
        cowboy_tcp_transport,
        [{port, Port}],
        cowboy_http_protocol,
        [{dispatch, Dispatch}]
    ).
