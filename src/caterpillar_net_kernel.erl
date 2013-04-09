-module(caterpillar_net_kernel).

-behaviour(gen_server).

-export([init/1, handle_info/2, handle_cast/2, handle_call/3, code_change/3, terminate/2]).
-export([start_link/1, stop/0, get_info/0]).


-record(state, {
    scan_nodes = [] :: [atom()],
    up_nodes = [] :: [atom()],
    down_nodes = [] :: [atom()],
    scan_timer :: reference()
}).

-define(DEFAULT_TIMEOUT, 5000).


%---------


start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).


stop() ->
    gen_server:call(?MODULE, stop, infinity).


get_info() ->
    gen_server:call(?MODULE, get_info, infinity).


%---------



init(Args) ->
    _ = os:cmd("epmd -daemon"),
    NodeName = proplists:get_value(self, Args),
    case net_kernel:start([NodeName, longnames]) of
        {ok, _} -> ok;
        Error ->
            error_logger:info_msg(
                "net_kernal failed to start with: ~p~n",
                [Error]
            ),
            init:stop()
    end,
    Cookie = proplists:get_value(cookie, Args, 'caterpillar'),
    true = erlang:set_cookie(node(), Cookie),
    ScanNodes = proplists:get_value(scan_nodes, Args, []),
    State = #state{scan_nodes=ScanNodes, down_nodes=ScanNodes},
    scan_nodes(1000, State),
    {ok, State}.


handle_call(get_info, _, #state{up_nodes=Up, down_nodes=Down}=State) ->
    Info = [{up_nodes, Up}, {down_nodes, Down}],
    {reply, Info, State};
handle_call(_, _, State) ->
    {reply, bad_msg, State}.


handle_info(scan_nodes, #state{down_nodes=DownNodes, up_nodes=UpNodes}=State) ->
    FoldFun = fun(Node, {Up, Down}) ->
        catch case net_adm:ping(Node) of
            pong -> 
                error_logger:info_msg("Node ~p up~n", [Node]),
                {[Node|Up], Down};
            pang ->
                error_logger:info_msg("Node ~p still down", [Node]),
                {Up, [Node|Down]};
            Other ->
                error_logger:error_msg("Bad node ~p, excluding f~n", [Node]),
                {Up, Down}
        end
    end,
    {Up, Down} = lists:foldl(FoldFun, {UpNodes, []}, DownNodes),
    {noreply, scan_nodes(State#state{down_nodes=Down, up_nodes=Up})};

handle_info(_, State) ->
    {noreply, State}.


handle_cast(_, State) ->
    {noreply, State}.


terminate(Reason, _State) ->
    error_logger:info_msg("caterpillar net kernel down witn ~p~n", [Reason]).


code_change(_Old, State, _Extra) ->
    {ok, State}.



%--------



-spec scan_nodes(#state{}) -> #state{}.
scan_nodes(State) ->
    scan_nodes(?DEFAULT_TIMEOUT, State).



scan_nodes(Delay, #state{scan_timer=Timer}=State) ->
    catch erlang:cancel_timer(Timer),
    State#state{
        scan_timer=erlang:send_after(Delay, ?MODULE, scan_nodes)
    }.



