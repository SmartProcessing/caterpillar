-module(caterpillar_event).

-behaviour(gen_server).

-include_lib("caterpillar_event_internal.hrl").

-export([start_link/1, stop/0]).
-export([sync_event/1, event/1]).
-export([register_service/1, register_worker/1]).
-export([init/1, handle_info/2, handle_cast/2, handle_call/3, code_change/3, terminate/2]).


start_link(_Args) -> gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).


stop() ->
    gen_server:call({global, ?MODULE}, stop, infinity).


event(Event) ->
    gen_server:cast({global, ?MODULE}, {event, Event}).


sync_event(Event) ->
    gen_server:call({global, ?MODULE}, {sync_event, Event}, infinity).


register_service(Type) ->
    gen_server:call({global, ?MODULE}, {register_service, Type}, infinity).


register_worker(Ident) ->
    gen_server:call({global, ?MODULE}, {register_worker, Ident}, infinity).


init(_) ->
    {ok, #state{ets=ets:new(?MODULE, [protected])}}.




handle_info({'DOWN', Ref, _Type, _Pid, _Reason}, #state{ets=Ets}=State) ->
    ets:delete(Ets, Ref),
    {noreply, State};

handle_info(_Msg, State) ->
    {noreply, State}.



handle_cast({event, _Event}, State) ->
    spawn(
        fun() -> ok end
    ),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.



handle_call({register, {Ident, Pid}}, _, #state{ets=Ets}=State) when is_pid(Pid) ->
    case ets:lookup(Ets, Ident) of 
        [] -> ok;
        _Something -> error_logger:info_msg("register warning: already got subscriber with this ident~n")
    end,
    ets:insert(Ets, {erlang:monitor(process, Pid), Ident, Pid}),
    {reply, ok, State};

handle_call({sync_event, {register, _Ident}}, {_Pid, _}=_From, State) ->
    {reply, ok, State};
handle_call({sync_event, _Event}, _From, State) ->
    {norely, State};


handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Msg, _From, State) ->
    {reply, {error, bad_msg}, State}.


code_change(_Old, State, _Extra) ->
    {ok, State}.


terminate(Reason, _State) ->
    error_logger:info_msg("caterpillar_router down with reason: ~p~n", [Reason]).
