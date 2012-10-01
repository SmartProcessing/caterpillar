-module(caterpillar_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    application:start(cowboy),
    caterpillar_sup:start_link().

stop(_State) ->
    application:stop(cowboy),
    application:stop(loggin),
    ok.
