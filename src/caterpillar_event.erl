-module(caterpillar_event).

-behaviour(gen_server).

-include_lib("caterpillar.hrl").
-include_lib("caterpillar_event_internal.hrl").
-define(
    SELECT(Type, ServiceOrIdent), 
    [{{'_', '$1', ServiceOrIdent, '$3'}, [{'==', '$1', Type}], ['$3']}]
).

-export([start_link/1, stop/0, get_info/0]).
-export([sync_event/1, event/1]).
-export([register_service/1, register_worker/2]).
-export([init/1, handle_info/2, handle_cast/2, handle_call/3, code_change/3, terminate/2]).
-export([register_name/2, unregister_name/1, whereis_name/1, send/2]).


-spec register_service(Type::term()) -> {ok, pid()} | {error, Reason :: term()}.
-spec register_worker(Ident::term(), WorkId :: non_neg_integer()) -> {ok, pid()} | {error, Reason :: term()}.
-spec get_info() -> [{worker|service, term()}].
-spec event(Event::term()) -> ok.
-spec sync_event(Event::term()) -> any().


%------ register name


register_name(Name, Pid) ->
    register(Name, Pid),
    global:register_name(Name, Pid).


unregister_name(Name) ->
    unregister(Name),
    global:unregister_name(Name).


send(Name, Msg) ->
    global:send(Name, Msg).


whereis_name(Name) ->
    global:whereis_name(Name).



%------



%doesnt work on erlang 14
%start_link(_Args) -> gen_server:start_link({via, ?MODULE, ?MODULE}, ?MODULE, [], []).
start_link(_Args) -> gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).


stop() ->
    gen_server:call({global, ?MODULE}, stop, infinity).


get_info() ->
    gen_server:call({global, ?MODULE}, get_info, infinity).


event(Event) ->
    gen_server:cast({global, ?MODULE}, {event, Event}).


sync_event(Event) ->
    gen_server:call({global, ?MODULE}, {sync_event, Event}, infinity).


register_service(Type) ->
    gen_server:call({global, ?MODULE}, {register_service, Type}, infinity).


register_worker(Ident, WorkId) ->
    gen_server:call({global, ?MODULE}, {register_worker, Ident, WorkId}, infinity).


init(_) ->
    register(?MODULE, self()),
    State = #state{ets = ets:new(?MODULE, [named_table, protected])},
    {ok, State}.



handle_info({'DOWN', Ref, _Type, Pid, _Reason}, #state{ets=Ets}=State) ->
    ets:delete(Ets, {Ref, Pid}),
    {noreply, State};

handle_info(_Msg, State) ->
    {noreply, State}.



handle_cast({event, {changes, _, _}=Event}, #state{ets=Ets}=State) ->
    spawn(fun() ->
        ForeachFun = fun(Pid) -> gen_server:cast(Pid, Event) end,
        lists:foreach(ForeachFun, select_workers_pids(Ets))
    end),
    {noreply, State};

handle_cast({event, {clean_packages, _}=Event}, #state{ets=Ets}=State) ->
    spawn(fun() ->
        ForeachFun = fun(Pid) -> gen_server:cast(Pid, Event) end,
        lists:foreach(ForeachFun, select_workers_pids(Ets))
    end),
    {noreply, State};

handle_cast({event, {Cmd, _}=Request}, #state{ets=Ets}=State) when
    Cmd == store_start_build;
    Cmd == store_progress_build;
    Cmd == store_error_build;
    Cmd == store_complete_build
->
    async_event_to_service(storage, Ets, Request),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.



handle_call({register_worker, Ident, WorkId}, {Pid, _}, #state{ets=Ets}=State) ->
    case catch select_worker(Ets, Ident) of
        {ok, _Pid} ->
            error_logger:info_msg(
                "register_worker warning: already got worker with ident ~p~n",
                [Ident]
            );
        _ -> ok
    end,
    ets:insert(Ets, {{erlang:monitor(process, Pid), Pid}, worker, Ident, Pid}),
    push_archives_to_new_worker(State, WorkId, Pid),
    {reply, {ok, self()}, State};

handle_call({register_service, Service}, {Pid, _}, #state{ets=Ets}=State) ->
    case select_service(Ets, Service) of
        {ok, SomePid} -> 
            error_logger:info_msg(
                "register_service warning: already got service ~p at ~p~n",
                [Service, SomePid]
            );
        _ -> ok
    end,
    ets:insert(Ets, {{erlang:monitor(process, Pid), Pid}, service, Service, Pid}),
    {reply, {ok, self()}, State};

handle_call({sync_event, {get_archive, #archive{}}=Request}, From, #state{ets=Ets}=State) ->
    sync_event_to_service(repository, From, Ets, Request),
    {noreply, State};

handle_call({sync_event, rescan_repository}, From, #state{ets=Ets}=State) ->
    sync_event_to_service(repository, From, Ets, rescan_repository),
    {noreply, State};

handle_call({sync_event, {notify, #notify{}}=Request}, From, #state{ets=Ets}=State) ->
    sync_event_to_service(notifier, From, Ets, Request),
    {noreply, State};

handle_call({sync_event, {deploy, #deploy{}}=Request}, From, #state{ets=Ets}=State) ->
    sync_event_to_service(deploy, From, Ets, Request),
    {noreply, State};

handle_call({sync_event, {rescan_package, #package{}}=Request}, From, #state{ets=Ets}=State) ->
    sync_event_to_service(repository, From, Ets, Request),
    {noreply, State};

handle_call({sync_event, {rebuild_package, #package{}}=Request}, From, #state{ets=Ets}=State) ->
    sync_event_to_service(repository, From, Ets, Request),
    {noreply, State};

handle_call({sync_event, {repository_custom_command, _Command, _Args}=Request}, From, #state{ets=Ets}=State) ->
    sync_event_to_service(repository, From, Ets, Request),
    {noreply, State};

handle_call({sync_event, {worker_custom_command, Command, Args, Ident}}, From, #state{ets=Ets}=State) ->
    spawn(fun() ->
        ForeachFun = fun(Pid) -> catch gen_server:call(Pid, {worker_custom_command, Command, Args}, 30000) end,
        Result = (catch lists:map(ForeachFun, select_workers_pids(Ets, Ident))),
        gen_server:reply(From, Result)
    end),
    {noreply, State};

handle_call(get_info, _From, #state{ets=Ets}=State) ->
    Info = [
        list_to_tuple(X) || X <- 
        ets:select(Ets, [{{'_', '$1', '$2', '_'}, [], [['$1', '$2']]}])
    ],
    {reply, Info, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(Msg, _From, State) ->
    error_logger:error_msg("bad msg in event: ~p~n", [Msg]),
    {reply, {error, bad_msg}, State}.


code_change(_Old, State, _Extra) ->
    {ok, State}.


terminate(Reason, _State) ->
    error_logger:info_msg("caterpillar_router down with reason: ~p~n", [Reason]).


%---------


select_service(Ets, Name) ->
    case catch ets:select(Ets, ?SELECT(service, Name)) of
        [] -> {error, no_service};
        [Pid|_] -> {ok, Pid};
        Error -> 
            error_logger:info_msg("select_service error: ~p~n", [Error]),
            {error, select_service}
    end.


select_worker(Ets, Ident) ->
    case catch ets:select(Ets, ?SELECT(worker, Ident)) of
        [] -> {error, no_worker};
        [Pid|_] -> {ok, Pid};
        Error ->  
            error_logger:info_msg("select_worker error: ~p~n", [Error]),
            {error, select_worker}
    end.
    

select_workers_pids(Ets) ->
    select_workers_pids(Ets, caterpillar_utils:any_ident()).


select_workers_pids(Ets, Ident) ->
    ets:select(Ets, ?SELECT(worker, Ident)).


push_archives_to_new_worker(#state{ets=Ets}, WorkId, WorkerPid) ->
    spawn(fun() ->
        Result = (catch begin
            {ok, RepPid } = select_service(Ets, repository),
            {ok, {changes, _, Archives}=Event} = gen_server:call(RepPid, {get_archives, WorkId}, infinity),
            case Archives of
                [] -> ok;
                _ -> gen_server:cast(WorkerPid, Event)
            end
        end),
        error_logger:info_msg("push_archives_to_new_worker result: ~p~n", [Result])
    end).
            

sync_event_to_service(Service, From, Ets, Request) ->
    spawn(fun() ->
        Reply = case catch select_service(Ets, Service) of
            {ok, Pid} -> catch gen_server:call(Pid, Request, infinity);
            Error -> Error
        end,
        gen_server:reply(From, Reply)
    end).

async_event_to_service(Service, Ets, Request) ->
    Reply = case catch select_service(Ets, Service) of
        {ok, Pid} -> catch gen_server:cast(Pid, Request);
        Error -> Error
    end.
