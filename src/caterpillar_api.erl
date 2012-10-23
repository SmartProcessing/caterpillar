-module(caterpillar_api).

-include_lib("caterpillar.hrl").
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/2]).
-export([start_link/1]).


start_link(_) -> ok.



init({tcp, http}, Req, Opts) ->
    {ok, Req, undefined_state}.



handle(Req, State) ->
    {ok, Req2} = cowboy_req:reply(200, [], <<"Hello World!">>, Req),
    {ok, Req2, State}.



terminate(_Req, _State) ->
    ok.
