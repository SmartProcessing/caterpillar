-module(caterpillar_api).

-include_lib("http.hrl").
-include_lib("caterpillar.hrl").
-behaviour(cowboy_http_handler).

-export([init/1, handle_call/3, handle_info/2, handle_cast/2, code_change/3]).
-export([init/3, handle/2, terminate/2]).
-export([start_link/1]).

-record(state, {ets}).


start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).


init(Args) ->
    ensure_started(cowboy),
    Ets = ets:new(?MODULE, [protected]),
    Dispatch = [
        {'_', [{'_', ?MODULE, []}]}
    ],
    Host = proplists:get_value(host, Args, "127.0.0.1"),
    Port = proplists:get_value(port, Args, 8088),
    cowboy:start_listener(?MODULE, 1,
        cowboy_tcp_transport, [{host, Host}, {port, Port}],
        cowboy_http_protocol, [{dispatch, Dispatch}]
    ),
    {ok, #state{ets=Ets}}.


handle_info({'DOWN', Ref, _, _, _}, #state{ets=Ets}=State) ->
    ets:match_delete(Ets, {'_', Ref}),
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.


handle_call({execute, {Cmd, Package, Branch}=Key}, _From, #state{ets=Ets}=State) ->
    Response = case ets:lookup(Ets, Key) of
        [] -> 
            Msg = {Cmd, {binary_to_list(Package), binary_to_list(Branch)}},
            case catch caterpillar_event:sync_event(Msg) of
                {ok, Pid} ->
                    Ref = erlang:monitor(process, Pid),
                    ets:insert(Ets, {Key, Ref}),
                    true;
                Error ->
                    {error, Error}
            end;
        _ -> false
    end, 
    {reply, Response, State};
handle_call(_, _, State) ->
    {reply, {error, bad_msg}, State}.


handle_cast(_, State) ->
    {noreply, State}.


code_change(_, State, _) ->
    {ok, State}.


%--------- cowboy & gen_server


terminate(_Req, _State) ->
    ok.


%--------- cowboy 


init({tcp, http}, Req, State) ->
    {ok, Req, State}.



handle(#http_req{path=[Cmd, Package, Branch]}=Req, State)
  when Cmd == <<"rescan">>; Cmd == <<"rebuild">>
->
    AtomCmd = binary_to_atom(<<Cmd/binary, "_package">>, latin1),
    Response = case gen_server:call(?MODULE, {execute, {AtomCmd, Package, Branch}}, infinity) of
        true ->
            {ok, Req2} = cowboy_http_req:reply(200, [], <<"ok">>, Req),
            Req2;
        false ->
            {ok, Req2} = cowboy_http_req:reply(200, [], <<"already in process">>, Req),
            Req2;
        Error -> 
            Res = list_to_binary(io_lib:format("~p~n", [Error])),
            {ok, Req2} = cowboy_http_req:reply(500, [], Res, Req),
            Req2
    end,
    {ok, Response, State};

handle(Req, State) ->
    {ok, Req2} = cowboy_http_req:reply(400, [], <<"bad request">>, Req),
    {ok, Req2, State}.



ensure_started(App) ->
    case application:start(App) of
        ok -> ok;
        {error, {already_started, _}} -> ok
    end.
