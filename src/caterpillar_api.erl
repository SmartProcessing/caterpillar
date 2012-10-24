-module(caterpillar_api).

-include_lib("http.hrl").
-include_lib("caterpillar.hrl").
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/2]).
-export([start_link/1]).

-record(state, {ets}).


start_link(Args) ->
    ensure_started(cowboy),
    Ets = ets=ets:new(?MODULE, [public]),
    State = #state{ets=Ets},
    Dispatch = [
        {'_', [{'_', ?MODULE, State}]}
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


init({tcp, http}, Req, State) ->
    {ok, Req, State}.



handle(#http_req{path=[Cmd, Package, Branch]}=Req, #state{ets=Ets}=State)
  when Cmd == <<"rescan">>; Cmd == <<"rebuild">>
->
    Response = (catch begin
        case ets:lookup(Ets, {Package, Branch}, self()) of
            [] -> ets:insert(Ets, {{Package, Branch}, self()});
            _ -> exit(in_process)
        end,
        AtomCmd = binary_to_atom(<<Cmd/binary, "_package">>, latin1),
        Msg = {AtomCmd, {binary_to_list(Package), binary_to_list(Branch)}},
        case catch caterpillar_event:sync_event(Msg) of
            {ok, _Pid} -> 
                {ok, Req2} = cowboy_http_req:reply(200, [], <<"ok">>, Req),
                {ok, Req2};
            Error ->
                Res = list_to_binary(io_lib:format("~p~n", [Error])),
                {ok, Req2} = cowboy_http_req:reply(500, [], Res, Req),
                {ok, Req2}
        end
    end),
    case Response of
        {ok, _} -> Response;
        Err->
            error_logger:error_msg("api: handle ~p error: ~p~n", [Cmd, Err]),
            cowboy_http_req:reply(500, [], Response, Req)
    end,
    ets:delete(Ets, {Package, Branch}),
    {ok, Response, State};

handle(Req, State) ->
    {ok, Req2} = cowboy_http_req:reply(400, [], <<"bad request">>, Req),
    {ok, Req2, State}.



terminate(_Req, _State) ->
    ok.
