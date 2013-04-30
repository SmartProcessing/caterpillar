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


handle_call({execute, Message}, _From, #state{ets=Ets}=State) ->
    Response = case ets:lookup(Ets, Message) of
        [] -> 
            case catch caterpillar_event:sync_event(Message) of
                {ok, Pid} ->
                    Ref = erlang:monitor(process, Pid),
                    ets:insert(Ets, {Message, Ref}),
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


init({tcp, http}, #http_req{path=Path}=Req, State) ->
    error_logger:info_msg("api request: ~ts~n", [string:join([binary_to_list(X) || X <- Path], "/")]),
    {ok, Req, State}.


handle(#http_req{path=[Cmd, Name|Rest]}=Req, State)
  when Cmd == <<"rescan">>; Cmd == <<"rebuild">>
->
    error_logger:info_msg("handling ~s~n", [Cmd]),
    AtomCmd = binary_to_atom(<<Cmd/binary, "_package">>, latin1),
    Package = #package{
        name = binary_to_list(Name),
        branch = (fun([Branch|_]) -> binary_to_list(Branch);([]) -> '_' end)(Rest)
    },
    Message = {execute,  {AtomCmd, Package}},
    Response = case gen_server:call(?MODULE, Message, infinity) of
        true ->
            {ok, Req2} = cowboy_http_req:reply(200, [], <<"ok\n">>, Req),
            Req2;
        false ->
            {ok, Req2} = cowboy_http_req:reply(200, [], <<"already in process\n">>, Req),
            Req2;
        Error -> 
            Res = format("~p~n", [Error]),
            {ok, Req2} = cowboy_http_req:reply(500, [], Res, Req),
            Req2
    end,
    {ok, Response, State};

handle(#http_req{path=[<<"init_repository">>, Path]}=Req, State) ->
    {ok, Req2} = case caterpillar_event:sync_event({repository_custom_command, init_repository, [Path]}) of
        {ok, Response} ->
            cowboy_http_req:reply(200, [], Response, Req);
        Error ->
            cowboy_http_req:reply(500, [], format("~p~n", [Error]), Req)
    end,
    {ok, Req2, State};

handle(#http_req{path=[<<"rebuild_dependencies">>, Name, Branch]}=Req, State) ->
    Args = [binary_to_list(X) || X <- [Name, Branch]],
    caterpillar_event:sync_event({worker_custom_command, rebuild_dependecies, Args}),
    {ok, cowboy_http_req:reply(200, [], <<"ok">>, Req), State};

handle(Req, State) ->
    error_logger:info_msg("bad request: ~p~n", [Req]),
    {ok, Req2} = cowboy_http_req:reply(400, [], <<"bad request\n">>, Req),
    {ok, Req2, State}.



ensure_started(App) ->
    case application:start(App) of
        ok -> ok;
        {error, {already_started, _}} -> ok
    end.



format(Template, Args) ->
    list_to_binary(io_lib:format(Template, Args)).
