-module(caterpillar_api).

-include_lib("caterpillar.hrl").

-export([init/1, handle_call/3, handle_info/2, handle_cast/2, code_change/3]).
-export([init/3, handle/2, terminate/2]).
-export([start_link/1]).
-export([terminate/3]).

-record(state, {ets}).


start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).


init(Args) ->
    ensure_started(ranch),
    ensure_started(cowlib),
    ensure_started(crypto),
    ensure_started(cowboy),
    Ets = ets:new(?MODULE, [protected]),
    Dispatch = cowboy_router:compile(
        [{'_', 
            [
                {'_', ?MODULE, []}
            ]}
        ]),
    Port = proplists:get_value(port, Args, 8088),
    cowboy:start_http(http, 100, [{port, Port}],
        [{env, [{dispatch, Dispatch}]}]),
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


init(_, Req, State) ->
    {Path, _} = cowboy_req:path(Req),
    error_logger:info_msg("api request: ~ts~n", 
        [Path]),
    {ok, Req, State}.

handle(Req, State) ->
    {Path, _} = cowboy_req:path(Req),
    handle_path(Path, Req, State).

terminate(_, _, _) ->
    ok.

handle_path(<<"rescan_repository">>, Req, State) ->
    Result = (catch caterpillar_event:sync_event(rescan_repository)),
    {ok, Req2} = cowboy_req:reply(200, [], Result, Req),
    {ok, Req2, State};

handle_path([Cmd, Name|Rest], Req, State)
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
            {ok, Req2} = cowboy_req:reply(200, [], <<"ok\n">>, Req),
            Req2;
        false ->
            {ok, Req2} = cowboy_req:reply(200, [], <<"already in process\n">>, Req),
            Req2;
        Error -> 
            Res = format("~p~n", [Error]),
            {ok, Req2} = cowboy_req:reply(500, [], Res, Req),
            Req2
    end,
    {ok, Response, State};

handle_path([<<"init_repository">>, Path], Req, State) ->
    {ok, Req2} = case caterpillar_event:sync_event({repository_custom_command, init_repository, [Path]}) of
        {ok, Response} ->
            cowboy_req:reply(200, [], Response, Req);
        Error ->
            cowboy_req:reply(500, [], format("~p~n", [Error]), Req)
    end,
    {ok, Req2, State};

handle_path([<<"rebuild_deps">>, Name, Branch|MaybeIdent], Req, State) ->
    Args = [binary_to_list(X) || X <- [Name, Branch]],
    Reply = case caterpillar_event:sync_event({worker_custom_command, rebuild_deps, Args, get_ident(MaybeIdent)}) of
        [{ok, Res}|_] ->
            ResIo = lists:map(fun(X) -> io_lib:format("~p", [X]) end, Res),
            list_to_binary("rebuilding dependencies:\n" ++ string:join(ResIo, ",\n") ++ "\n");
        Reason ->
            error_logger:error_msg("error: rebuild_dependencies: ~p~n", [Reason]),
            list_to_binary(io_lib:format("error: ~p~n", [Reason]))
    end,
    {ok, Req2} = cowboy_req:reply(200, [], Reply, Req),
    {ok, Req2, State};

handle_path([<<"pkg_info">>, Name, Branch|MaybeIdent], Req, State) ->
    Result = caterpillar_event:sync_event({worker_custom_command, pkg_info, [Name, Branch], get_ident(MaybeIdent)}),
    Reply = lists:foldl(
        fun
            ({ok, Res}, Acc) ->
                Info = lists:flatten(io_lib:format(
                    "Name: ~s~nBranch: ~s~nTag: ~s~nState: ~p~nDepends: ~p~nHas in dependencies: ~p~n", 
                    [?GV("name", Res), ?GV("branch", Res), ?GV("tag", Res), ?GV("state", Res), ?GV("depends", Res), ?GV("has_in_deps", Res)]
                )),
                Bin = list_to_binary(Info),
                <<Bin/binary, "\n", Acc/binary>>;
            (Error, Acc) ->
                Reason = case Error of
                    {error, Rsn} -> Rsn;
                    _ -> {bad_result, Error}
                end,
                Bin = list_to_binary(io_lib:format("error: ~p~n", [Reason])),
                <<Bin/binary, "\n", Acc/binary>>
        end,
        <<>>,
        Result
    ), 
    {ok, Req2} = cowboy_req:reply(200, [], Reply, Req),
    {ok, Req2, State};

% Storage requests

handle_path([<<"storage">>, IdentType, IdentArch, Cmd|Args], Req, State) ->
    {ok, Req2} = case catch jsonx:encode(caterpillar_event:sync_event({storage, Cmd, [{IdentType, IdentArch}|Args]})) of
        Response when is_binary(Response) ->
            cowboy_req:reply(200, [], Response, Req);
        Error ->
            cowboy_req:reply(500, [], format("~p~n", [Error]), Req)
    end,
    {ok, Req2, State};

handle_path(Path, Req, State) ->
    error_logger:info_msg("bad request: ~p~n", [Path]),
    {ok, Req2} = cowboy_req:reply(400, [], <<"bad request\n">>, Req),
    {ok, Req2, State}.


ensure_started(App) ->
    case application:start(App) of
        ok -> ok;
        {error, {already_started, _}} -> ok
    end.



format(Template, Args) ->
    list_to_binary(io_lib:format(Template, Args)).


get_ident([Type, Arch]) -> caterpillar_utils:gen_ident(Type, Arch);
get_ident(_) -> caterpillar_utils:any_ident().
