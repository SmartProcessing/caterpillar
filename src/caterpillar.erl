-module(caterpillar).
-behaviour(gen_server).
-define(GV, proplists:get_value).

-record(state, {
        plugins=[],
        stats,
        unpack_dir,
        main_queue,
        wait_queue,
        next_to_build,
        workers=[]
    }).

-type rev_def() :: {atom(), atom(), list()}.

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
    VCSPlugins = ?GV(vcs_plugin, Settings),
    Plugins = lists:map(fun({Plugin, PluginSettings}) -> 
                {ok, Pid} = Plugin:start(PluginSettings),
                Pid end, VCSPlugins),
    Unpack = ?GV(unpack_dir, Settings),
    Stats = dets:open_file(
        ?GV(stats, Settings, "/var/lib/smprc/caterpillar/stats")),
    BuildQueue = queue:new(),
    WaitQueue = queue:new(),
    WorkerList = create_workers(?GV(build_workers_number, Settings, 5)),
    {ok, #state{
            plugins=Plugins,
            stats=Stats,
            unpack_dir=Unpack,
            main_queue=BuildQueue,
            wait_queue=WaitQueue,
            workers=WorkerList,
            next_to_build=none
        }}.

handle_call({newref, RevDef}, _From, State) ->
    Queue = queue:in(RevDef, State#state.main_queue),
    %% ...
    number_free_workers(State),
    list_building_revs(State),
    %% TODO
    {reply, State#state{main_queue=Queue}, State};
handle_call({built, _Worker}, _From, State) ->
    schedule_build(State),
    {reply, ok, State};
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
        _Other -> %%TODO
            {ok, State}
    end.

-spec create_workers(WorkerNumber :: non_neg_integer()) -> {ok, [{Pid :: pid(), none}]}.
create_workers(WorkerNumber) ->
    create_workers(WorkerNumber, []).
create_workers(0, Acc) ->
    Acc;
create_workers(WorkerNumber, Acc) ->
    Pid = erlang:spawn_monitor(fun build_worker/0),
    create_workers(WorkerNumber - 1, [{Pid, none}|Acc]).

build_worker() ->
    receive
        build ->
            not_implemented;
        _Other ->
            not_implementer
    end,
    build_worker().

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
