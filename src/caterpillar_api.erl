-module(caterpillar_api).

-include_lib("caterpillar.hrl").
-include_lib("http.hrl").
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/2]).
-export([start_link/1]).


start_link(Args) ->
    ensure_started(cowboy),
    Dispatch = [
        {'_', [{'_', ?MODULE, []}]}
    ],
    Host = proplists:get_value(host, Args, "127.0.0.1"),
    Port = proplists:get_value(port, Args, 8088),
    cowboy:start_listener(?MODULE, 1,
        cowboy_tcp_transport, [{host, Host}, {port, Port}],
        cowboy_http_protocol, [{dispatch, Dispatch}]
    ).


ensure_started(App) ->
    case application:start(App) of
        ok -> ok;
        {error, {already_started, _}} -> ok
    end.


init({tcp, http}, Req, Opts) ->
    {ok, Req, []}.



handle(#http_req{path=[Cmd, Package, Branch]}=Req, State)
  when Cmd == <<"rescan">>; Cmd == <<"rebuild">>
->
    AtomCmd = binary_to_atom(<<Cmd/binary, "_package">>, latin1),
    Msg = {AtomCmd, {binary_to_list(Package), binary_to_list(Branch)}},
    case catch caterpillar_event:sync_event(Msg) of
        ok -> 
            {ok, Req2} = cowboy_http_req:reply(200, [], <<"ok">>, Req),
            {ok, Req2, State};
        Error ->
            Response = list_to_binary(io_lib:format("~p~n", [Error])),
            {ok, Req2} = cowboy_http_req:reply(500, [], Response, Req),
            {ok, Req2, State}
    end;
            

handle(#http_req{path=[<<"rebuild">>, Package, Branch]}=Req, State) ->
    {ok, Req2} = cowboy_http_req:reply(200, [], <<"ok">>, Req),
    {ok, Req2, State};


handle(Req, State) ->
    {ok, Req2} = cowboy_http_req:reply(400, [], <<"bad request">>, Req),
    {ok, Req2, State}.



terminate(_Req, _State) ->
    ok.
