% @doc simple lock with queues

-module(caterpillar_lock).
-behaviour(gen_server).

-export([start_link/0, init/1, 
        handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3, stop/0]).

-record(state, {storage}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).


init([]) ->
    Storage = ets:new('storage', []),
    register(caterpillar_lock, self()),
    {ok, #state{storage=Storage}}.


stop() ->
    gen_server:call(?MODULE, stop).

    
handle_call({lock, Ident}, From, State) ->
    case ets:lookup(State#state.storage, Ident) of
        [{Ident, true, SomeRef, Q}] ->
            ets:insert(State#state.storage, {Ident, true, SomeRef, queue:in({From}, Q)}),
            {noreply, State};
        [{Ident, false, _, _}] ->
            ets:insert(State#state.storage, {Ident, true, From, queue:new()}),
            {reply, ok, State};
        [] ->
            ets:insert(State#state.storage, {Ident, true, From, queue:new()}),
            {reply, ok, State}
    end;

handle_call({unlock, Ident}, From, State) ->
    unlock_ref(Ident, From, State),
    {reply, ok, State};

handle_call({state, Ident}, _From, State) ->
    {reply, ets:lookup(State#state.storage, Ident), State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Msg, _From, State) ->
    {reply, unknown, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.

    
handle_info({unlock, Ident, From}, State) ->
    error_logger:info_msg("Lock warning: unlocking ~p by timeout~n", [Ident]),
    unlock_ref(Ident, From, State),
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


unlock_ref(Ident, From, State) ->
    [{Ident, _, _Source, Q}] = ets:lookup(State#state.storage, Ident),
    error_logger:info_msg("ulocking ~p~n", [{Ident, From}]),
    case queue:is_empty(Q) of
        false ->
            {{value, {Client}}, NewQ} = queue:out(Q),
            ets:insert(State#state.storage, {Ident, true, Client, NewQ}),
            catch gen_server:reply(Client, ok);
        true ->
            ets:insert(State#state.storage, {Ident, false, none, Q})
    end.
