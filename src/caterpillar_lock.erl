% @doc simple lock with queues

-module(caterpillar_lock).
-behaviour(gen_server).

-export([start_link/0, init/1, 
        handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

-record(state, {storage}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).


init([]) ->
    Storage = ets:new('storage', []),
    register(caterpillar_lock, self()),
    {ok, #state{storage=Storage}}.

    
handle_call({lock, Ident, Timeout}, From, State) ->
    case ets:lookup(State#state.storage, Ident) of
        [{Ident, true, SomeRef, Q}] ->
            ets:insert(State#state.storage, {Ident, true, SomeRef, queue:in({From, Timeout}, Q)}),
            {noreply, State};
        [{Ident, false, _, _}] ->
            ets:insert(State#state.storage, {Ident, true, From, queue:new()}),
            erlang:send_after(Timeout, self(), {unlock, Ident, From}),
            {reply, ok, State};
        [] ->
            ets:insert(State#state.storage, {Ident, true, From, queue:new()}),
            erlang:send_after(Timeout, self(), {unlock, Ident, From}),
            {reply, ok, State}
    end;

handle_call({unlock, Ident}, From, State) ->
    unlock_ref(Ident, From, State),
    {reply, ok, State};

handle_call({state, Ident}, _From, State) ->
    {reply, ets:lookup(State#state.storage, Ident), State};

handle_call(_Msg, _From, State) ->
    {reply, unknown, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.

    
handle_info({unlock, Ident, From}, State) ->
    unlock_ref(Ident, From, State),
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


unlock_ref(Ident, From, State) ->
    [{Ident, _, Source, Q}] = ets:lookup(State#state.storage, Ident),
    case Source == From of
        true ->
            case queue:is_empty(Q) of
                false ->
                    {{value, {Client, Timeout}}, NewQ} = queue:out(Q),
                    ets:insert(State#state.storage, {Ident, true, Client, NewQ}),
                    erlang:send_after(Timeout, self(), {unlock, Ident, Client}),
                    gen_server:reply(Client, ok);
                true ->
                    ets:insert(State#state.storage, {Ident, false, none, Q})
            end.
        false ->
            {error, prohibited}
    end.
