-module(caterpillar_app).

-behaviour(application).

-export([start/0, stop/0]).
-export([start/2, stop/1]).


start() ->
    application:start(caterpillar).


stop() ->
    application:stop(caterpillar).


start(_StartType, _StartArgs) ->
    caterpillar_sup:start_link().


stop(_State) ->
    ok.
