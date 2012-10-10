-module(caterpillar_event).

-behaviour(gen_server).

-include_lib("caterpillar.hrl").
-include_lib("caterpillar_event_internal.hrl").
-define(
    SELECT(Type, ServiceOrIdent), 
    [{{'_', '$1', '$2', '$3'}, [{'andalso', {'==', '$1', Type}, {'==', '$2', ServiceOrIdent}}], ['$3']}]
).

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
    State = #state{
        ets = ets:new(?MODULE, [protected]) %{ref, type, pid, ident|service}
    },
    {ok, State}.



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



handle_call({sync_event, {register_worker, Ident}}, {Pid, _}, #state{ets=Ets}=State) ->
    case ets:select(Ets, ?SELECT(worker, Ident)) of
        [] ->
            ok;
        _Something ->
            error_logger:info_msg(
                "register_worker warning: already got worker with ident ~p~n",
                [Ident]
            )
    end,
    ets:insert(Ets, {erlang:monitor(process, Pid), worker, Ident, Pid}),
    {reply, ok, State};

handle_call({sync_event, {register_service, Service}}, {Pid, _}, #state{ets=Ets}=State) ->
    case ets:select(Ets, ?SELECT(service, Service)) of
        [] -> ok;
        [SomePid|_] ->
            error_logger:info_msg(
                "register_service warning: already got service ~p at ~p~n",
                [Service, SomePid]
            )
    end,
    ets:insert(Ets, {erlang:monitor(process, Pid), service, Service, Pid}),
    {reply, ok, State};

handle_call({sync_event, {get_archive, #archive{}}=Request}, From, #state{ets=Ets}=State) ->
    spawn(fun() ->
        Reply = case catch select_service(Ets, repository) of
            {ok, Pid} -> catch gen_server:call(Pid, Request, infinity);
            Error -> Error
        end,
        gen_server:reply(From, Reply)
    end),
    {noreply, State};

handle_call({sync_event, {notify, #notify{}}=Request}, From, #state{ets=Ets}=State) ->
    spawn(fun() ->
        Reply = case catch select_service(Ets, notifier) of
            {ok, Pid} -> catch gen_server:call(Pid, Request, infinity);
            Error -> Error
        end,
        gen_server:reply(From, Reply)
    end),
    {noreply, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Msg, _From, State) ->
    {reply, {error, bad_msg}, State}.


code_change(_Old, State, _Extra) ->
    {ok, State}.


terminate(Reason, _State) ->
    error_logger:info_msg("caterpillar_router down with reason: ~p~n", [Reason]).





%---------





select_service(Ets, Name) ->
    case catch ets:select(Ets, ?SELECT(service, Name)) of
        [] ->
            error_logger:info_msg("select_service: no service ~p available~n", [Name]),
            {error, no_service};
        [Pid|_] -> 
            {ok, Pid};
        Error -> 
            error_logger:info_msg("select_service error: ~p~n", [Error]),
            {error, select_service}
    end.


select_worker(Ets, Name) ->
    case catch ets:select(Ets, ?SELECT(worker, Name)) of
        [] ->
            error_logger:info_msg("select worker: no worker ~p available~n", [Name]),
            {error, no_worker};
        [Pid|_] -> 
            {ok, Pid};
        Error -> 
            error_logger:info_msg("select_worker error: ~p~n", [Error]),
            {error, select_worker}
    end.
    

