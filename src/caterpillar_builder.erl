-module(caterpillar_builder).
-include_lib("caterpillar.hrl").
-include_lib("caterpillar_builder_internal.hrl").
-behaviour(gen_server).
-export([start_link/1, state/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, prepare/4, deploy/3]).

-define(CPU, caterpillar_pkg_utils).
-define(CU, caterpillar_utils).
-define(CBS, caterpillar_build_storage).
-define(UNPACK_RETRY_LIMIT, 15).
-define(BTL, binary_to_list).
-define(LTB, list_to_binary).

-record(state, {
    master_pid=none,
    deps,
    main_queue,
    wait_queue,
    queue_switch=true,
    next_to_build,
    workers=[],
    unpack_state,
    build_path,
    poll_time,
    queue_missing=false,
    prebuild=[],
    queued=[],
    work_id,
    wid,
    ident
}).


start_link(Settings) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Settings, []).


state() ->
    gen_server:call(?MODULE, state, 10000).


init(Settings) ->
    error_logger:info_msg("starting caterpillar_builder~n", []),
    DepsFile = ?GV(deps, Settings, ?DEFAULT_DEPENDENCIES_DETS),
    filelib:ensure_dir(DepsFile),
    {ok, Deps} = dets:open_file(deps, [{file, DepsFile}]),
    ?CBS:cleanup_new_in_progress(Deps),
    PollTime = ?GV(poll_time, Settings, 10000),
    BuildQueue = queue:new(),
    WaitQueue = queue:new(),
    QueueMissing = ?GV(queue_missing, Settings, false),
    {ok, WorkerList} = create_workers({?GV(build_workers_number, Settings, 5), Settings}),
    error_logger:info_msg("workers initialized"),
    UnpackState = ets:new(unpack_state, []),
    BuildPath = ?GV(build_path, Settings, ?DEFAULT_BUILD_PATH),
    ?CBS:cleanup_statpack_dir(BuildPath),
    WorkIdFile = ?GV(work_id, Settings, ?DEFAULT_WORK_ID_FILE),
    WorkId = ?CU:read_work_id(WorkIdFile),
    {Type, Arch}=?GV(ident, Settings, {unknown, unknown}),
    Ident = caterpillar_utils:gen_ident(Type, Arch),
    start_timer(PollTime),
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
        wid = WorkId,
        ident=Ident
    }}.

handle_call({newref, RevDef}, _From, State) ->
    error_logger:info_msg("received new revision: ~p~n", [?VERSION(RevDef)]),
    ?CBS:create_dep(State#state.deps, RevDef),
    Queue = queue:in(RevDef, State#state.main_queue),
    QueuedState = State#state{main_queue=Queue},
    case State#state.next_to_build of
        none ->
            {ok, NewState} = schedule_build(QueuedState);
        _Other ->
            {ok, NewState} = try_build(QueuedState)
    end,
    {reply, ok, NewState};
handle_call({built, Worker, RevDef, BuildInfo}, _From, State=#state{ident=Ident}) ->
    ?CBS:update_dep_state(State#state.deps, RevDef, <<"built">>),
    caterpillar_event:event({store_complete_build, [
        {Ident#ident.type, Ident#ident.arch},
        {RevDef#rev_def.name, RevDef#rev_def.branch}, 
        RevDef#rev_def.work_id,
        ?LTB(BuildInfo#build_info.pkg_name),
        BuildInfo#build_info.description 
    ]}),
    erlang:spawn(?MODULE, deploy, [RevDef, BuildInfo, State]),
    NewWorkers = release_worker(Worker, State#state.workers),
    {ok, ScheduledState} = schedule_build(State#state{workers=NewWorkers}),
    {ok, NewState} = try_build(ScheduledState),
    {reply, ok, NewState};
handle_call({err_built, Worker, RevDef, BuildInfo}, _From, #state{ident=Ident}=State) ->
    error_logger:info_msg("error built: ~p~n", [BuildInfo]),
    caterpillar_event:event({store_error_build, [
        {Ident#ident.type, Ident#ident.arch},
        {RevDef#rev_def.name, RevDef#rev_def.branch}, 
        RevDef#rev_def.work_id, BuildInfo#build_info.state,
        BuildInfo#build_info.description
    ]}),
    notify(BuildInfo#build_info.state,
        RevDef#rev_def.work_id,
        Ident,
        ?VERSION(RevDef),
        BuildInfo#build_info.description),
    ?CBS:update_dep_state(State#state.deps, RevDef, BuildInfo#build_info.state),
    NewWorkers = release_worker(Worker, State#state.workers),
    {ok, ScheduledState} = schedule_build(State#state{workers=NewWorkers}),
    {ok, NewState} = try_build(ScheduledState),
    {reply, ok, NewState};
handle_call(state, _From, State) ->
    {reply, State, State};
handle_call({worker_custom_command, rebuild_deps, [Name, Branch|_]}, _From, State) ->
    Vsn = {?LTB(Name), ?LTB(Branch), <<>>},
    Res = case ?CBS:get_subj(State#state.deps, Vsn) of
        Subj when is_list(Subj) ->
            lists:map(fun(X) -> self() ! {rebuild, X}
            end, Subj),
            Subj;
        _Other ->
            []
    end,
    {reply, {ok, Res}, State};
handle_call({worker_custom_command, pkg_info, [Name, Branch|_]}, _From, State) ->
    Vsn = {Name, Branch, <<>>},
    Res = case ?CBS:fetch_dep_non_block(State#state.deps, Vsn) of
        {ok, {{N, B, T}, Status, Obj, Subj}} ->
            {ok, [
                {"name", N},
                {"branch", B},
                {"tag", T},
                {"state", Status},
                {"depends", Obj},
                {"has_in_deps", Subj}
            ]};
        _Other ->
            {error, <<"not found\n">>}
    end,
    {reply, Res, State};
handle_call(Request, _From, State) ->
    error_logger:info_msg("unknown message: ~p~n", [Request]),
    {reply, unknown, State}.

handle_cast({changes, WorkId, Archives}, State) ->
    error_logger:info_msg("changes for ~p arrived~n", [WorkId]),
    ?CU:write_work_id(State#state.work_id, WorkId),
    ToPreprocess = lists:usort(Archives),
    {ok, NewState} = process_archives(ToPreprocess, State, WorkId),
    {noreply, NewState#state{wid = WorkId}};
handle_cast({clean_packages, Archives}, State) ->
    Fun = fun(Archive) ->
        Version = {
            list_to_binary(Archive#archive.name),
            list_to_binary(Archive#archive.branch),
            <<>>
        },
        ?CBS:delete(State#state.deps, State#state.build_path, Version)
    end,
    lists:map(Fun, Archives),
    {noreply, State};
handle_cast(Msg, State) ->
    {noreply, State}.

handle_info(timer_loop, State) ->
    self() ! schedule,
    start_timer(State#state.poll_time),
    {noreply, State};
handle_info({'DOWN', _, _, Pid, _}, State) when Pid == State#state.master_pid ->
    {noreply, State#state{master_pid=none}};
handle_info({'DOWN', Reference, _, _, Reason}, State) ->
    Preparing = case Reason of
        normal ->
            State#state.queued;
        _Other ->
            case ets:lookup(State#state.unpack_state, Reference) of
                [{Reference, Version}|_] ->
                error_logger:error_msg("preprocess on ~p failed: ~p~n", [Version, Reason]),
                lists:delete(Version, State#state.queued);
            _Other ->
                State#state.queued
            end
    end,
    ets:delete(State#state.unpack_state, Reference),
    {noreply, State#state{queued=Preparing}};
handle_info(schedule, State) when State#state.master_pid == none ->
    error_logger:info_msg("trying to register self~n"),
    case catch caterpillar_event:register_worker(State#state.ident, State#state.wid) of
        {ok, Pid} ->
            error_logger:info_msg("registered worker at ~p~n", [Pid]),
            erlang:monitor(process, Pid),
            {noreply, State#state{master_pid=Pid}};
        Other ->
            error_logger:error_msg("couldn't register self~n", [Other]),
            {noreply, State}
    end;
handle_info(schedule, State) when State#state.master_pid /= none ->
    case State#state.prebuild of
        [{_V, Archive, WorkId}|Rest] ->
            {ok, NewState} = process_archives([Archive], State#state{prebuild=Rest}, WorkId);
        [] ->
            NewState = State
    end,
    {ok, SState} = schedule_build(NewState),
    {noreply, SState};
handle_info({rebuild, Version}, State) ->
    Archive = ?CPU:get_version_archive(Version),
    Prebuild = lists:ukeysort(1, [{Version, Archive, State#state.wid}|State#state.prebuild]),
    {noreply, State#state{prebuild=Prebuild}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% preprocessing
process_archives([], State, _WorkId) -> {ok, State};
process_archives([A|O], #state{unpack_state=UnpackState, build_path=BuildPath, ident=Ident}=State, WorkId) ->
    Vsn = ?CPU:get_archive_version(A),
    {Preparing, Prebuild} = case can_prepare(A, State) of
        true ->
            process_archive(BuildPath, A, UnpackState, WorkId, Ident),
            {[Vsn|State#state.queued], State#state.prebuild};
        _ ->
            {State#state.queued, lists:ukeysort(1, [{Vsn, A, WorkId}|State#state.prebuild])}
    end,
    process_archives(O, State#state{queued=Preparing, prebuild=Prebuild}, WorkId).


process_archive(BuildPath, Archive, UnpackState, WorkId, Ident) ->
    {_Pid, Monitor} = erlang:spawn_monitor(?MODULE, prepare, [BuildPath, Archive, WorkId, Ident]),
    ets:insert(UnpackState, {Monitor, ?CPU:get_archive_version(Archive)}).


prepare(BuildPath, Archive, WorkId, Ident) ->
    Vsn = ?CPU:get_archive_version(Archive),
    TempArch = ?CBS:get_temp_path(BuildPath, Vsn),
    {ok, Fd} = file:open(TempArch, [read, write]),
    ArchiveWithFd = Archive#archive{fd=Fd},
    Msg = {get_archive, ArchiveWithFd},
    case caterpillar_event:sync_event(Msg) of
        {ok, NewArchive} ->
            unpack_archive(BuildPath, NewArchive, WorkId, Ident);
        Other ->
            error_logger:error_msg("failed to get archive ~p:~p~n", [Archive, Other]),
            exit(no_archive)
    end.


unpack_archive(Prefix, Archive, WorkId, Ident) ->
    Vsn = ?CPU:get_archive_version(Archive),
    Type = Archive#archive.archive_type,
    Cwd = ?CBS:get_path(Prefix, Vsn, <<"new">>),
    catch ?CU:del_dir(Cwd),
    filelib:ensure_dir(Cwd ++ "/empty"),
    ok = caterpillar_archive:extract(?CBS:get_temp_path(Prefix, Vsn), [{type, Type}, {cwd, Cwd}]),
    file:close(Archive#archive.fd),
    file:delete(?CBS:get_temp_path(Prefix, Vsn)),
    case ?CPU:get_pkg_config(Archive, Cwd) of
        {error, Reason} ->
            Body = io_lib:format("failed to parse pkg.config: ~p~n", [Reason]),
            notify("error",
                WorkId,
                Ident,
                Vsn,
                Body),
            exit(no_pkg_config);
        PkgRecord = #pkg_config{} ->
            RevDef = ?CPU:pack_rev_def(Archive, PkgRecord, WorkId),
            caterpillar_event:event({store_start_build, [
                {Ident#ident.type, Ident#ident.arch},
                {RevDef#rev_def.name, RevDef#rev_def.branch}, 
                RevDef#rev_def.work_id,
                Archive#archive.current_revno
            ]}),
            gen_server:call(caterpillar_builder, {newref, RevDef}, infinity)
    end.


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
    
-spec create_workers({WorkerNumber :: non_neg_integer(), Settings :: term()}) -> {ok, [{Pid :: pid(), none}]}.
create_workers({WorkerNumber, Settings}) ->
    error_logger:info_msg(
        "starting ~B build workers, worker supervisor and lock storage: ~p ~n", 
        [WorkerNumber, caterpillar_build_worker_sup:start_link(Settings)]
    ),
    caterpillar_lock_sup:start_link(),
    create_workers(WorkerNumber, []).


create_workers(0, Acc) -> {ok, Acc};
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
    FoldFun = fun
        ({_Pid, none}, Cnt) -> Cnt + 1; 
        ({_Pid, _Other}, Cnt) -> Cnt
    end,
    {ok, lists:foldl(FoldFun, 0, State#state.workers)}.


release_worker(Worker, Workers) ->
    MapFun =  fun
        ({W, _V}) when W == Worker -> {Worker, none};
        (Other) -> Other
    end,
    lists:map(MapFun, Workers).


%% Scheduling
%% ------------------------------------------------------------------


-spec start_timer(non_neg_integer()) -> ok.
start_timer(Timeout) ->
    erlang:send_after(Timeout, self(), timer_loop).

-spec schedule_build(State :: #state{}) -> {ok, NewState :: #state{}}.
schedule_build(State) when State#state.next_to_build == none ->
    {ok, NewState} = get_build_candidate(State),
    try_build(NewState);
schedule_build(State) ->
    try_build(State).


-spec get_build_candidate(State :: #state{}) -> {ok, NewState :: #state{}}.
get_build_candidate(State) ->
    #state{main_queue = MainQueue, wait_queue = WaitQueue} = State,
    case {queue:is_empty(WaitQueue), queue:is_empty(MainQueue)} of
        {true, true} -> {ok, State};
        {true, false} -> get_build_candidate(main_queue, State);
        {false, true} -> get_build_candidate(wait_queue, State);
        {false, false} -> get_build_candidate(both, State)
    end.

get_build_candidate(QType, State) when QType == main_queue; QType == wait_queue ->
    {{value, Candidate}, Queue} = extract_candidate(QType, State),
    case check_build_deps(Candidate, State) of
        independent ->
            submit_next_to_build(QType, Queue, Candidate, State);
        dependent ->
            case QType of
                main_queue ->
                    {ok, State#state{main_queue=Queue, wait_queue=queue:in(Candidate, State#state.wait_queue)}};
                wait_queue ->
                    {ok, State#state{wait_queue=queue:in(Candidate, Queue)}}
            end;
        _Other ->
            submit_missing(QType, Queue, Candidate, State)
    end;
get_build_candidate(both, State) ->
    {ok, NewState} = case State#state.queue_switch of
        true ->
            get_build_candidate(wait_queue, State);
        false ->
            get_build_candidate(main_queue, State)
    end,
    SW = not(State#state.queue_switch),
    {ok, NewState#state{queue_switch=SW}}.

extract_candidate(QType, State) ->
    case QType of
        main_queue ->
            queue:out(State#state.main_queue);
        wait_queue ->
            queue:out(State#state.wait_queue)
    end.

submit_next_to_build(QType, Queue, Candidate, State=#state{ident=Ident}) ->
    ?CBS:update_dep_state(State#state.deps, Candidate, <<"in_progress">>),
    caterpillar_event:event({store_progress_build, [
        {Ident#ident.type, Ident#ident.arch},
        {Candidate#rev_def.name, Candidate#rev_def.branch}, 
        Candidate#rev_def.work_id,
        ?LTB(Candidate#rev_def.pkg_config#pkg_config.description)
    ]}),
    case QType of
        main_queue ->
            {ok, State#state{
                main_queue=Queue, 
                next_to_build=Candidate,
                queued=lists:delete(?VERSION(Candidate), State#state.queued)
            }};
        wait_queue ->
            {ok, State#state{
                wait_queue=Queue, 
                next_to_build=Candidate,
                queued=lists:delete(?VERSION(Candidate), State#state.queued)
            }}
    end.

submit_missing(QType, Queue, Candidate, State) ->
    ?CBS:update_dep_state(State#state.deps, Candidate, <<"none">>),
    case QType of
        main_queue ->
            {ok, State#state{
                main_queue=Queue, 
                queued=lists:delete(?VERSION(Candidate), State#state.queued)
            }};
        wait_queue ->
            {ok, State#state{
                wait_queue=Queue, 
                queued=lists:delete(?VERSION(Candidate), State#state.queued)
            }}
    end.


%% External communication
%% ------------------------------------------------------------------

-spec notify(State :: list(), WorkId :: integer(), Ident :: #ident{}, Version :: version(), Body :: list()) -> ok|error.

notify(State, WorkId, Ident, {N, B, T}, Body) ->
    Subj = io_lib:format("#~B ~s: ~s/~s~s at ~s/~s", [
            WorkId,
            State,
            ?BTL(N),
            ?BTL(B),
            format_tag(T), 
            Ident#ident.type,
            Ident#ident.arch
        ]),
    notify(Subj, Body).

-spec get_notify_message(Subject::binary()|string(), Body::binary()|string()) -> #notify{}.
get_notify_message(Subject, Body) ->
    ToBin = fun
        (X) when is_list(X) -> list_to_binary(X);
        (X) when is_binary(X) -> X
    end,
    #notify{subject=ToBin(Subject), body=ToBin(Body)}.


-spec notify(list(), list()) -> ok.
notify(Subject, Body) ->
    Notify = get_notify_message(Subject, Body),
    caterpillar_event:sync_event({notify, Notify}).

-spec check_build_deps(Candidate :: #rev_def{}, State :: #state{}) -> 
    independent|dependent|missing|{error, term()}.
check_build_deps(Candidate, State) ->
    Preparing = State#state.queued,
    case ?CBS:list_unres_deps(State#state.deps, Candidate, Preparing) of
        {ok, [], []} ->
            {ok, NowBuilding} = list_building_revs(State),
            {ok, Res} = ?CBS:check_isect(
                Candidate,
                NowBuilding),
            Res;
        {ok, [], Deps} ->
            dependent;
        {ok, Dependencies, _} when is_list(Dependencies) ->
            missing_actions(State#state.queue_missing, Dependencies, State, Candidate);
        {error, Res} ->
            {error, Res};
        Other ->
            {error, Other}
    end.

missing_actions(true, Dependencies, State, _) ->
    ask_for_rebuild(Dependencies, State),
    dependent;
missing_actions(_, Dependencies, State=#state{ident=Ident}, Candidate) ->
    Body = io_lib:format("missing dependencies: ~p~n", [Dependencies]),
    notify("error", 
        Candidate#rev_def.work_id,
        State#state.ident,
        ?VERSION(Candidate),
        Body),
    caterpillar_event:event({store_error_build, [
        {Ident#ident.type, Ident#ident.arch},
        {Candidate#rev_def.name, Candidate#rev_def.branch}, 
        Candidate#rev_def.work_id,
        list_to_binary(Body)
    ]}),
    missing.

%% Utils
%%

ask_for_rebuild(Dependencies, State) ->
    lists:map(
        fun(Dep) ->
            {BPackage, BBranch, _} = Dep,
            QueuedState = lists:member(Dep, State#state.queued),
            PrebuildState = lists:keymember(Dep, 1, State#state.prebuild),
            {ok, {_, BS, _, _}} = ?CBS:fetch_dep(State#state.deps, Dep),
            if (not QueuedState) and (not PrebuildState)
                and (BS == <<"missing">>)
                ->
                    error_logger:info_msg("ask for rebuild~p~n", [Dep]),
                self() ! {rebuild, Dep};
            true ->
                pass
            end
        end, Dependencies).


deploy(RevDef, BuildInfo, #state{ident=#ident{arch=Arch, type=Type}=Ident}=State) ->
    DeployPkg = #deploy_package{
        name = binary_to_list(RevDef#rev_def.name),
        branch = binary_to_list(RevDef#rev_def.branch),
        package = BuildInfo#build_info.pkg_name,
        fd = BuildInfo#build_info.fd
    },
    Subj = io_lib:format("#~B success: ~s/~s~s at ~s/~s", [
        RevDef#rev_def.work_id,
        binary_to_list(RevDef#rev_def.name),
        binary_to_list(RevDef#rev_def.branch),
        format_tag(RevDef#rev_def.tag),
        Type,
        Arch
    ]),
    Message = io_lib:format(
        "Mime-Version: 1.0\n"
        "Content-type: text/html; charset=\"utf-8\"\n"
        "built package: ~s\n"
        "", [BuildInfo#build_info.pkg_name]
    ),
    Deploy = #deploy{
        ident=Ident,
        work_id=RevDef#rev_def.work_id,
        packages=[DeployPkg],
        post_deploy_actions = [{caterpillar_event, sync_event, [{notify, get_notify_message(Subj, Message)}]}]
    },
    caterpillar_event:sync_event({deploy, Deploy}).


format_tag([]) -> "";
format_tag(<<>>) -> "";
format_tag(undefined) -> "";
format_tag(Tag) when is_binary(Tag) -> format_tag(binary_to_list(Tag));
format_tag(Tag) when is_list(Tag) -> "/" ++ Tag;
format_tag(Tag) -> lists:flatten(io_lib:format("/~p", [Tag])).
