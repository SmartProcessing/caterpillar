-module(caterpillar).
-behaviour(gen_server).
-define(GV, proplists:get_value).

-record(state, {
        plugins=[],
        deps,
        unpack_dir,
        main_queue,
        wait_queue,
        next_to_build,
        workers=[]
    }).

-type rev_def() :: {atom(), atom(), list()}.
-type plugin_def() :: {PluginName :: atom(), PluginArguments :: [term()]}.

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Settings) ->
    VCSPlugins = ?GV(vcs_plugins, Settings),
    Plugins = init_plugins(VCSPlugins),
    Unpack = ?GV(unpack_dir, Settings),
    Deps = dets:open_file(
        ?GV(deps, Settings, "/var/lib/smprc/caterpillar/stats")),
    BuildQueue = queue:new(),
    WaitQueue = queue:new(),
    {ok, WorkerList} = create_workers(?GV(build_workers_number, Settings, 5)),
    {ok, #state{
            plugins=Plugins,
            deps=Deps,
            unpack_dir=Unpack,
            main_queue=BuildQueue,
            wait_queue=WaitQueue,
            workers=WorkerList,
            next_to_build=none
        }}.

handle_call({newref, RevDef}, _From, State) ->
    Queue = queue:in(RevDef, State#state.main_queue),
    QueuedState = State#state{main_queue=Queue},
    case State#state.next_to_build of
        none ->
            {ok, NewState} = schedule_build(QueuedState);
        _Other ->
            {ok, NewState} = try_build(QueuedState)
    end,
    {reply, ok, NewState};
handle_call({built, _Worker}, _From, State) ->
    {ok, ScheduledState} = schedule_build(State),
    {ok, NewState} = try_build(ScheduledState),
    {reply, ok, NewState};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% Build section
%% ------------------------------------------------------------------

-spec try_build(State :: record()) -> {ok, NewState :: record()}.
try_build(State) ->
    case number_free_workers(State) of
        Int when Int > 0 ->
            job_free_worker(State);
        _Other ->
            {ok, State}
    end.

-spec job_free_worker(State :: record()) -> {ok, NewState :: record()}.
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
    caterpillar_worker_sup:start_link(),
    create_workers(WorkerNumber, []).
create_workers(0, Acc) ->
    Acc;
create_workers(WorkerNumber, Acc) ->
    {ok, Pid} = supervisor:start_child(caterpillar_worker_sup, []),
    create_workers(WorkerNumber - 1, [{Pid, none}|Acc]).

-spec list_building_revs(State :: record()) -> {ok, [rev_def()]}.
list_building_revs(State) ->
    {ok, [RevDef || {_Pid, RevDef} <- State#state.workers, RevDef /= none]}.

-spec number_free_workers(State :: record()) -> {ok, non_neg_integer()}.
number_free_workers(State) ->
    {ok, lists:foldl(
            fun({_Pid, none}, Cnt) -> 
                    Cnt + 1; 
                ({_Pid, _Other}, Cnt) ->
                    Cnt
            end, State#state.workers)}.


%% Scheduling section
%% ------------------------------------------------------------------

-spec schedule_build(State :: record()) -> {ok, NewState :: record()}.
schedule_build(State) when State#state.next_to_build == none ->
    get_build_candidate(State);
schedule_build(State) ->
    {ok, State}.

-spec get_build_candidate(State :: record()) -> {ok, NewState :: record()}.
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
        true -> %% independent
            {ok, State#state{main_queue=MainQueue, next_to_build=Candidate}};
        false ->
            WaitQueue = queue:in(Candidate, State#state.wait_queue),
            get_build_candidate(
                State#state{main_queue=MainQueue, wait_queue=WaitQueue})
    end;
get_build_candidate(wait_queue, State) ->
    {{value, Candidate}, WaitQueue} = queue:out(State#state.wait_queue),
    case check_build_deps(Candidate, State) of
        true -> %% independent
            {ok, State#state{wait_queue=WaitQueue, next_to_build=Candidate}};
        false ->
            WaitQueue = queue:in(Candidate, State#state.wait_queue),
            get_build_candidate(
                State#state{wait_queue=WaitQueue})
    end;
get_build_candidate(both, State) ->
    {{value, Candidate}, WaitQueue} = queue:out(State#state.wait_queue),
    case check_build_deps(Candidate, State) of
        true -> %% independent
            {ok, State#state{wait_queue=WaitQueue, next_to_build=Candidate}};
        false ->
            WaitQueue = queue:in(Candidate, State#state.wait_queue),
            get_build_candidate(main_queue,
                State#state{wait_queue=WaitQueue})
    end.


%% External communication section
%% ------------------------------------------------------------------

-spec init_plugins(VCSPlugins :: [plugin_def()]) -> [Pid :: pid()].
init_plugins(VCSPlugins) ->
    lists:map(
        fun({Plugin, PluginSettings}) -> 
                {ok, Pid} = Plugin:start(PluginSettings),
                erlang:monitor(Pid),
                Pid 
        end, VCSPlugins).

%% @doc true when candidate is independent from building packages
-spec check_build_deps(Candidate :: rev_def(), State :: record()) -> true|false.
check_build_deps(Candidate, State) ->
    {ok, NowBuilding} = list_building_revs(State),
    case caterpillar_dependencies:check_list(
            State#state.deps,
            Candidate,
            NowBuilding
        ) of
        {ok, [{depends, []}, {is_dependencie, []}]} ->
            true;
        _Other ->
            false
    end.
