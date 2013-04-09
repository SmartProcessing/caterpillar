-module(caterpillar_builder).
-include_lib("caterpillar.hrl").
-include_lib("caterpillar_internal.hrl").
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, prepare/3]).

-define(CPU, caterpillar_pkg_utils).
-define(CU, caterpillar_utils).
-define(CDEP, caterpillar_dependencies).
-define(UNPACK_RETRY_LIMIT, 15).

-record(state, {
        master_state=false,
        deps,
        main_queue,
        wait_queue,
        next_to_build,
        workers=[],
        unpack_state,
        build_path,
        poll_time,
        queue_missing=false,
        prebuild=[],
        queued=[],
        work_id,
        ident
    }).


start_link(Settings) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Settings, []).

init(Settings) ->
    error_logger:info_msg("starting caterpillar_builder~n", []),
    {ok, Deps} = dets:open_file(deps,
        [{file, ?GV(deps, Settings, ?DEFAULT_DEPENDENCIES_DETS)}]),
    PollTime = ?GV(poll_time, Settings, 10000),
    BuildQueue = queue:new(),
    WaitQueue = queue:new(),
    QueueMissing = ?GV(queue_missing, Settings, false),
    {ok, WorkerList} = create_workers({?GV(build_workers_number, Settings, 5), Settings}),
    error_logger:info_msg("workers initialized"),
    UnpackState = ets:new(unpack_state, []),
    BuildPath = ?GV(build_path, Settings, ?DEFAULT_BUILD_PATH),
    WorkIdFile = ?GV(work_id, Settings, ?DEFAULT_WORK_ID_FILE),
    WorkId = get_work_id(WorkIdFile),
    Ident = ?GV(ident, Settings, "unknown"),
    schedule_poller(PollTime),
    {ok, #state{
            deps=Deps,
            main_queue=BuildQueue,
            wait_queue=WaitQueue,
            workers=WorkerList,
            next_to_build=none,
            unpack_state=UnpackState,
            build_path=BuildPath,
            queue_missing=QueueMissing,
            poll_time=PollTime,
            work_id=WorkIdFile,
            ident=Ident
        }
    }.

handle_call({newref, RevDef}, _From, State) ->
    error_logger:info_msg("received new revision: ~p~n", [?VERSION(RevDef)]),
    ?CDEP:create_dependencie(State#state.deps, RevDef),
    Queue = queue:in(RevDef, State#state.main_queue),
    QueuedState = State#state{main_queue=Queue},
    case State#state.next_to_build of
        none ->
            {ok, NewState} = schedule_build(QueuedState);
        _Other ->
            {ok, NewState} = try_build(QueuedState)
    end,
    {reply, ok, NewState};
handle_call({built, Worker, RevDef, BuildInfo}, _From, State) ->
    ?CDEP:update_dependencies(State#state.deps, RevDef, <<"built">>),
    DeployPkg = #deploy_package{
        name = binary_to_list(RevDef#rev_def.name),
        branch = binary_to_list(RevDef#rev_def.branch),
        package = BuildInfo#build_info.pkg_name,
        fd = BuildInfo#build_info.fd
    },
    Deploy = #deploy{
        ident=State#state.ident,
        work_id=RevDef#rev_def.work_id,
        packages=[DeployPkg]
    },
    caterpillar_event:sync_event({deploy, Deploy}),
    Subj = io_lib:format("#~B success: ~s/~s/~s", [
            RevDef#rev_def.work_id,
            binary_to_list(RevDef#rev_def.name),
            binary_to_list(RevDef#rev_def.branch),
            binary_to_list(RevDef#rev_def.tag)
        ]),
    notify(Subj, "ok"),
    NewWorkers = release_worker(Worker, State#state.workers),
    {ok, ScheduledState} = schedule_build(State#state{workers=NewWorkers}),
    {ok, NewState} = try_build(ScheduledState),
    {reply, ok, NewState};
handle_call({err_built, Worker, RevDef, BuildInfo}, _From, State) ->
    error_logger:info_msg("error built: ~p~n", [BuildInfo]),
    Subj = io_lib:format("#~B error: ~s/~s/~s", [
            RevDef#rev_def.work_id,
            binary_to_list(RevDef#rev_def.name),
            binary_to_list(RevDef#rev_def.branch),
            binary_to_list(RevDef#rev_def.tag)
        ]),
    notify(Subj, BuildInfo#build_info.description),
    BuildState = BuildInfo#build_info.state,
    ?CDEP:update_dependencies(State#state.deps, RevDef, BuildState),
    NewWorkers = release_worker(Worker, State#state.workers),
    {ok, ScheduledState} = schedule_build(State#state{workers=NewWorkers}),
    {ok, NewState} = try_build(ScheduledState),
    {reply, ok, NewState};
handle_call(state, _From, State) ->
    {reply, State, State};
handle_call(_Request, _From, State) ->
    {reply, unknown, State}.

handle_cast({changes, WorkId, Archives}, State) ->
    update_work_id(State#state.work_id, WorkId),
    ToPreprocess = lists:usort(Archives),
    {ok, NewState} = process_archives(ToPreprocess, State, WorkId),
    {noreply, NewState};
handle_cast({rebuild_deps, WorkId, Version}, State) ->
    DepArchives = case ?CDEP:fetch_dependencies(State#state.deps, Version) of
        {ok, []} ->
            [];
        {ok, {_, {St, _}, _Object, Subject}} when St == built; St == tested ->
            lists:map(fun(X) -> ?CPU:get_version_archive(X) end, Subject);
        Other ->
            error_logger:info_msg("wrong: ~p~n", [Other]),
            []
    end,
    error_logger:info_msg("rebuilding ~p dependencies: ~p~n", [Version, DepArchives]),
    {ok, NewState} = process_archives(DepArchives, State, WorkId),
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', Reference, _, _, Reason}, State) ->
    case Reason of
        normal ->
            Preparing = State#state.queued,
            ok;
        _Other ->
            [{Reference, Archive}|_] = ets:lookup(State#state.unpack_state, Reference),
            error_logger:error_msg("preprocess on ~p failed: ~p~n", [Archive, Reason]),
            Preparing = lists:delete(Archive, State#state.queued),
            Subj = io_lib:format("#Unpack error: ~p", [Archive]),
            Body = io_lib:format("Failed to unpack archive ~p: ~p~n", [Archive, Reason]),
            notify(Subj, Body)
    end,
    ets:delete(State#state.unpack_state, Reference),
    {noreply, State#state{queued=Preparing}};
handle_info(schedule, State#state{master_state=false}) ->
    case catch caterpillar_event:register_worker(caterpillar_builder, State#state.work_id) of
        {ok, _} ->
            {noreply, State#state{master_state=true}};
        Other ->
            error_logger:error_msg("couldn't register self: ~p~n", [Other])
            {noreply, State};
    end;
handle_info(schedule, State#state{master_state=true}) ->
    {ok, ScheduledState} = schedule_build(State),
    case State#state.prebuild of
        [{Archive, WorkId}|Rest] ->
            {ok, NewState} = process_archives([Archive], ScheduledState#state{prebuild=Rest}, WorkId);
        [] ->
            NewState = ScheduledState
    end,
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Preprocessing
process_archives([], State, _WorkId) ->
    {ok, State};
process_archives([A|O], State, WorkId) ->
    UnpackState = State#state.unpack_state,
    BuildPath = State#state.build_path,
    case can_prepare(A, State) of
        true ->
            Preparing = [?CPU:get_archive_version(A)|State#state.queued],
            Prebuild = State#state.prebuild,
            process_archive(BuildPath, A, UnpackState, WorkId);
        false ->
            Preparing = State#state.queued,
            Prebuild = lists:ukeysort(1, [{A, WorkId}|State#state.prebuild])
    end,
    process_archives(O, State#state{queued=Preparing, prebuild=Prebuild}, WorkId).

process_archive(BuildPath, Archive, UnpackState, WorkId) ->
    {_Pid, Monitor} = erlang:spawn_monitor(
        ?MODULE, 
        prepare, 
        [BuildPath, Archive, WorkId]),
    ets:insert(UnpackState, {Monitor, ?CPU:get_archive_version(Archive)}).


prepare(BuildPath, Archive, WorkId) ->
    error_logger:info_msg("queued archive: ~p~n", [Archive]),
    TempName = io_lib:format(
        "~s-~s~s",
        [Archive#archive.name, Archive#archive.branch, Archive#archive.tag]
    ),
    TempArch = filename:join([BuildPath, "temp", TempName]) ++ ".tar",
    filelib:ensure_dir(TempArch),
    {ok, Fd} = file:open(TempArch, [read, write]),
    ArchiveWithFd = Archive#archive{fd=Fd},
    Msg = {get_archive, ArchiveWithFd},
    ok = caterpillar_event:sync_event(Msg),
    Cwd = filename:join([BuildPath, "temp", TempName]) ++ "/",
    catch ?CU:del_dir(Cwd),
    ok = caterpillar_tar:extract(TempArch, [{cwd, Cwd}, compressed]),
    file:delete(TempArch),
    PkgRecord = ?CPU:get_pkg_config(Archive, Cwd),
    RevDef = ?CPU:pack_rev_def(Archive, PkgRecord, WorkId),
    gen_server:call(caterpillar_builder, {newref, RevDef}, infinity).


can_prepare(Archive, State) ->
    Version = ?CPU:get_archive_version(Archive),
    Next = State#state.next_to_build,
    {ok, BuildRevs} = list_building_revs(State),
    BuildVsns = [?VERSION(Rev) || Rev <- [Next|BuildRevs], Rev /= none],
    InQueues = State#state.queued,
    not(lists:member(Version, InQueues ++ BuildVsns)).


%% Build
%% ------------------------------------------------------------------

-spec try_build(State :: #state{}) -> {ok, NewState :: #state{}}.
try_build(State) ->
    case number_free_workers(State) of
        {ok, Int} when Int > 0 ->
            job_free_worker(State);
        _Other ->
            {ok, State}
    end.

-spec job_free_worker(State :: #state{}) -> {ok, NewState :: #state{}}.
job_free_worker(State) ->
    NewWorkers = job_free_worker(
        State#state.workers, State#state.next_to_build),
    {ok, State#state{workers=NewWorkers, next_to_build=none}}.

job_free_worker(Workers, none) ->
    Workers;
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
    
-spec create_workers({WorkerNumber :: non_neg_integer(), Settings :: term()}) -> 
    {ok, [{Pid :: pid(), none}]}.
create_workers({WorkerNumber, Settings}) ->
    error_logger:info_msg("starting ~B build workers, worker supervisor and lock storage: ~p ~n", 
        [WorkerNumber, caterpillar_build_worker_sup:start_link(Settings)]),
    caterpillar_lock_sup:start_link(),
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
            end, 0, State#state.workers)}.

release_worker(Worker, Workers) ->
    lists:map(
        fun({W, _V}) when W==Worker ->
            {Worker, none};
        (Other) ->
            Other
        end, Workers).


%% Scheduling
%% ------------------------------------------------------------------


-spec schedule_poller(non_neg_integer()) -> ok.
schedule_poller(Timeout) ->
    erlang:send_after(Timeout, self(), schedule).

-spec schedule_build(State :: #state{}) -> {ok, NewState :: #state{}}.
schedule_build(State) when State#state.next_to_build == none ->
    {ok, NewState} = get_build_candidate(State),
    schedule_poller(State#state.poll_time),
    try_build(NewState);
schedule_build(State) ->
    schedule_poller(State#state.poll_time),
    try_build(State).

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
    error_logger:info_msg("checking ~p~n", [?VERSION(Candidate)]),
    case check_build_deps(Candidate, State) of
        independent ->
            error_logger:info_msg("next candidate: ~p~n", [?VERSION(Candidate)]),
            ?CDEP:update_dependencies(State#state.deps, Candidate, <<"in_progress">>),
            {ok, State#state{
                    main_queue=MainQueue, 
                    next_to_build=Candidate,
                    queued=lists:delete(?VERSION(Candidate), State#state.queued)
                }};

        dependent ->
            WaitQueue = queue:in(Candidate, State#state.wait_queue),
            {ok, State#state{main_queue=MainQueue, wait_queue=WaitQueue}};
        missing ->
            {ok, State#state{
                    main_queue=MainQueue,
                    queued=lists:delete(?VERSION(Candidate), State#state.queued)
                }};
        _Other ->
            RolledMainQueue = queue:in(Candidate, MainQueue),
            {ok, State#state{main_queue=RolledMainQueue}}
    end;
get_build_candidate(wait_queue, State) ->
    {{value, Candidate}, WaitQueue} = queue:out(State#state.wait_queue),
    case check_build_deps(Candidate, State) of
        independent ->
            ?CDEP:update_dependencies(State#state.deps, Candidate, <<"in_progress">>),
            {ok, State#state{
                    wait_queue=WaitQueue, 
                    queued=lists:delete(?VERSION(Candidate), State#state.queued),
                    next_to_build=Candidate}};
        dependent ->
            RolledWaitQueue = queue:in(Candidate, WaitQueue),
            {ok, State#state{wait_queue=RolledWaitQueue}};
        missing ->
            {ok, State#state{
                    queued=lists:delete(?VERSION(Candidate), State#state.queued),
                    wait_queue=WaitQueue}};
        Other ->
            Subject = io_lib:format("Build for ~p", [Candidate]),
            Body = io_lib:format("Error while processing dependencies: ~n~p~n", [Other]),
            notify(Subject, Body),
            {error, broken_deps}
    end;
get_build_candidate(both, State) ->
    {{value, Candidate}, WaitQueue} = queue:out(State#state.wait_queue),
    case check_build_deps(Candidate, State) of
        independent ->
            ?CDEP:update_dependencies(State#state.deps, Candidate, <<"in_progress">>),
            {ok, State#state{
                    queued=lists:delete(?VERSION(Candidate), State#state.queued),
                    wait_queue=WaitQueue, 
                    next_to_build=Candidate}};
        dependent ->
            RolledWaitQueue = queue:in(Candidate, WaitQueue),
            get_build_candidate(main_queue,
                State#state{wait_queue=RolledWaitQueue});
        missing ->
            {ok, State#state{
                    wait_queue=WaitQueue,
                    queued=lists:delete(?VERSION(Candidate), State#state.queued)
                }};
        _Other ->
            {error, broken_deps}
    end.


%% External communication
%% ------------------------------------------------------------------
-spec notify(list(), list()) -> ok.
notify(Subject, Body) ->
    Msg = {notify, #notify{
            subject=list_to_binary(Subject), 
            body=list_to_binary(Body)}},
    caterpillar_event:sync_event(Msg).

-spec check_build_deps(Candidate :: #rev_def{}, State :: #state{}) -> 
    independent|dependent|missing|{error, term()}.
check_build_deps(Candidate, State) ->
    Preparing = State#state.queued,
    case ?CDEP:list_unresolved_dependencies(
            State#state.deps, Candidate, Preparing) of
        {ok, [], []} ->
            {ok, NowBuilding} = list_building_revs(State),
            {ok, Res} = ?CDEP:check_intersection(
                Candidate,
                NowBuilding),
            Res;
        {ok, [], _Deps} ->
            dependent;
        {ok, Dependencies, _} when is_list(Dependencies) ->
            case State#state.queue_missing of
                true ->
                    lists:map(
                        fun(Dep) ->
                                {BPackage, BBranch, _} = Dep,
                                QueuedState = lists:member(Dep, State#state.queued),
                                if not QueuedState ->
                                    Msg = {rebuild_package, {binary_to_list(BPackage), binary_to_list(BBranch)}},
                                    caterpillar_event:sync_event(Msg);
                                true ->
                                    pass
                                end
                        end, Dependencies),
                    dependent;
                _Other ->
                    Subj = io_lib:format("#~B error: ~s/~s/~s", [
                            Candidate#rev_def.work_id,
                            binary_to_list(Candidate#rev_def.name),
                            binary_to_list(Candidate#rev_def.branch),
                            binary_to_list(Candidate#rev_def.tag)
                        ]),
                    Body = io_lib:format("Missing dependencies: ~p~n", [Dependencies]),
                    notify(Subj, Body),
                    missing
            end;
        {error, Res} ->
            {error, Res};
        Other ->
            {error, Other}
    end.

%% Utils

get_work_id(File) ->
    case file:consult(File) of
        {ok, [Id]} when is_integer(Id) ->
            Id;
        _Other ->
            update_work_id(File, 0)
    end.

update_work_id(File, Id) when is_integer(Id) ->
    BStrId = list_to_binary(io_lib:format("~B.", [Id])),
    {ok, Fd} = file:open(File, [write]),
    file:write(Fd, BStrId),
    file:close(Fd),
    Id.
