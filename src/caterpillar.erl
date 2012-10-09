-module(caterpillar).
-include_lib("caterpillar.hrl").
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, prepare/3]).

-record(state, {
        deps,
        main_queue,
        wait_queue,
        next_to_build,
        workers=[],
        unpack_state,
        event_service,
        build_path
    }).


start_link(Settings) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Settings, []).

init(Settings) ->
    error_logger:info_msg("starting caterpillar", []),
    EventService = ?GV(event_service, Settings, caterpillar_event),
    error_logger:info_msg("plugins initialized"),
    {ok, Deps} = dets:open_file(deps,
        [{file, ?GV(deps, Settings, "/var/lib/smprc/caterpillar/deps")}]),
    BuildQueue = queue:new(),
    WaitQueue = queue:new(),
    {ok, WorkerList} = create_workers(?GV(build_workers_number, Settings, 5)),
    error_logger:info_msg("workers initialized"),
    caterpillar_api:start(),
    error_logger:info_msg("api started"),
    UnpackState = ets:new(unpack_state, []),
    {ok, #state{
            deps=Deps,
            main_queue=BuildQueue,
            wait_queue=WaitQueue,
            workers=WorkerList,
            next_to_build=none,
            unpack_state=UnpackState,
            event_service=EventService
        }
    }.

handle_call({newref, RevDef, _RevInfo}, _From, State) ->
    Queue = queue:in(RevDef, State#state.main_queue),
    QueuedState = State#state{main_queue=Queue},
    case State#state.next_to_build of
        none ->
            {ok, NewState} = schedule_build(QueuedState);
        _Other ->
            {ok, NewState} = try_build(QueuedState)
    end,
    {reply, ok, NewState};
handle_call({built, _Worker, RevDef, _BuildInfo}, _From, State) ->
    caterpillar_dependencies:update(State#state.deps, RevDef),
    {ok, ScheduledState} = schedule_build(State),
    {ok, NewState} = try_build(ScheduledState),
    {reply, ok, NewState};
handle_call({err_built, _Worker, _RevDef, _BuildInfo}, _From, State) ->
    {ok, ScheduledState} = schedule_build(State),
    {ok, NewState} = try_build(ScheduledState),
    {reply, ok, NewState};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({work, _Id, Archives}, State) ->
    process_archives(Archives, State),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', Reference, _, _, Reason}, State) when Reason /= normal ->
    [{Reference, _Archive}|_] = ets:lookup(State#state.unpack_state, Reference),
    %%TODO repeat some actions with archive, smth failed
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Preprocessing
process_archives([], _State) ->
    {ok, done};
process_archives([A|O], State) ->
    UnpackState = State#state.unpack_state,
    BuildPath = State#state.build_path,
    EventService = State#state.event_service,
    {ok, _Pid, ToRepeat} = erlang:spawn_monitor(
        ?MODULE, 
        prepare, 
        [BuildPath, A, EventService]),
    ets:insert(UnpackState, {ToRepeat, A}),
    process_archives(O, State).


prepare(BuildPath, Archive, EventService) ->
    TempName = io_lib:format("~s-~s", [
            Archive#archive.name, 
            Archive#archive.branch
        ]
    ),
    TempArch = BuildPath ++ "/temp/" ++ TempName ++ ".tar",
    Fd = file:open(TempArch, [read, write]),
    ArchiveWithFd = Archive#archive{archive=Fd},
    {ok, ArchiveWithFd} = gen_server:call(
        EventService, 
        {get_archive, ArchiveWithFd}, 
        infinity),
    Cwd = BuildPath ++ "/temp/" ++ TempName,
    erl_tar:extract(Fd, [{cwd, BuildPath ++ "/temp/" ++ TempName}]),
    PkgConfig = caterpillar_utils:get_pkg_config(Cwd),
    Deps = caterpillar_utils:get_dep_list(PkgConfig),
    RevDef = caterpillar_utils:pack_rev_def(Archive, Deps),
    gen_server:call(caterpillar, {newref, RevDef}).
    

%% Build
%% ------------------------------------------------------------------

-spec try_build(State :: #state{}) -> {ok, NewState :: #state{}}.
try_build(State) ->
    case number_free_workers(State) of
        Int when Int > 0 ->
            job_free_worker(State);
        _Other ->
            {ok, State}
    end.

-spec job_free_worker(State :: #state{}) -> {ok, NewState :: #state{}}.
job_free_worker(State) ->
    NewWorkers = job_free_worker(
        State#state.workers, State#state.next_to_build),
    {ok, State#state{workers=NewWorkers}}.

job_free_worker([{Pid, none}|Other], ToBuild) ->
    gen_server:cast(Pid, {build, ToBuild}),
    [{Pid, ToBuild}|Other];
job_free_worker([{Pid, SomeRef}|Other], ToBuild) ->
    job_free_worker(Other, ToBuild, [{Pid, SomeRef}]).

job_free_worker([{Pid, none}|Other], ToBuild, OldW) ->
    gen_server:cast(Pid, {build, ToBuild}),
    [{Pid, ToBuild}|Other] ++ OldW;
job_free_worker([{Pid, SomeRef}|Other], ToBuild, OldW) ->
    job_free_worker(Other, ToBuild, [{Pid, SomeRef}|OldW]).
    
-spec create_workers(WorkerNumber :: non_neg_integer()) -> 
    {ok, [{Pid :: pid(), none}]}.
create_workers(WorkerNumber) ->
    error_logger:info_msg("starting ~B build workers and worker supervisor: ~p ~n", 
        [WorkerNumber, caterpillar_build_worker_sup:start_link()]),
    create_workers(WorkerNumber, []).
create_workers(0, Acc) ->
    {ok, Acc};
create_workers(WorkerNumber, Acc) ->
    case supervisor:start_child(caterpillar_build_worker_sup, []) of
        {ok, Pid} ->
            error_logger:info_msg("build worker started at ~p", [Pid]),
            create_workers(WorkerNumber - 1, [{Pid, none}|Acc]);
        Other ->
            error_logger:error_msg("failed to start build worker: ~p", [Other]),
            {error, error}
    end.

-spec list_building_revs(State :: #state{}) -> {ok, [#rev_def{}]}.
list_building_revs(State) ->
    {ok, [RevDef || {_Pid, RevDef} <- State#state.workers, RevDef /= none]}.

-spec number_free_workers(State :: #state{}) -> {ok, non_neg_integer()}.
number_free_workers(State) ->
    {ok, lists:foldl(
            fun({_Pid, none}, Cnt) -> 
                    Cnt + 1; 
                ({_Pid, _Other}, Cnt) ->
                    Cnt
            end, State#state.workers)}.


%% Scheduling
%% ------------------------------------------------------------------

-spec schedule_build(State :: #state{}) -> {ok, NewState :: #state{}}.
schedule_build(State) when State#state.next_to_build == none ->
    get_build_candidate(State);
schedule_build(State) ->
    {ok, State}.

-spec get_build_candidate(State :: #state{}) -> {ok, NewState :: #state{}}.
get_build_candidate(State) ->
    case {
            queue:is_empty(State#state.wait_queue),
            queue:is_empty(State#state.main_queue)} of
        {true, true} ->
            {ok, State};
        {true, false} ->
            get_build_candidate(main_queue, State);
        {false, true} ->
            get_build_candidate(wait_queue, State);
        {false, false} ->
            get_build_candidate(both, State)
    end.

get_build_candidate(main_queue, State) ->
    {{value, Candidate}, MainQueue} = queue:out(State#state.main_queue),
    case check_build_deps(Candidate, State) of
        independent ->
            {ok, State#state{main_queue=MainQueue, next_to_build=Candidate}};
        dependent ->
            WaitQueue = queue:in(Candidate, State#state.wait_queue),
            get_build_candidate(
                State#state{main_queue=MainQueue, wait_queue=WaitQueue});
        _Other ->
            RolledMainQueue = queue:in(Candidate, MainQueue),
            get_build_candidate(State#state{main_queue=RolledMainQueue})
    end;
get_build_candidate(wait_queue, State) ->
    {{value, Candidate}, WaitQueue} = queue:out(State#state.wait_queue),
    case check_build_deps(Candidate, State) of
        independent -> %% independent
            {ok, State#state{wait_queue=WaitQueue, next_to_build=Candidate}};
        dependent ->
            WaitQueue = queue:in(Candidate, State#state.wait_queue),
            get_build_candidate(
                State#state{wait_queue=WaitQueue});
        _Other ->
            {error, broken_deps}
    end;
get_build_candidate(both, State) ->
    {{value, Candidate}, WaitQueue} = queue:out(State#state.wait_queue),
    case check_build_deps(Candidate, State) of
        independent -> %% independent
            {ok, State#state{wait_queue=WaitQueue, next_to_build=Candidate}};
        dependent ->
            WaitQueue = queue:in(Candidate, State#state.wait_queue),
            get_build_candidate(main_queue,
                State#state{wait_queue=WaitQueue});
        _Other ->
            {error, broken_deps}
    end.


%% External communication
%% ------------------------------------------------------------------
-spec check_build_deps(Candidate :: #rev_def{}, State :: #state{}) -> true|false.
check_build_deps(Candidate, State) ->
    case caterpillar_dependencies:list_unresolved_dependencies(
            State#state.deps, Candidate) of
        {ok, []} ->
            {ok, NowBuilding} = list_building_revs(State),
            {ok, Res} = caterpillar_dependencies:check_intersection(
                Candidate,
                NowBuilding),
            Res;
        {ok, Dependencies} when is_list(Dependencies) ->
            {unresolved, Dependencies};
        {error, Res} ->
            {error, Res};
        Other ->
            {error, Other}
    end.
