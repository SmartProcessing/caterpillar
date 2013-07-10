-module(caterpillar_storage).

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(DEFAULT_STORAGE, "/var/lib/smprc/caterpillar/storage").
-define(DEFAULT_WORK_ID_FILE, "/var/lib/smprc/caterpillar/storage_work_id").
-define(DTU, smprc_datetime_utils).
-define(LIST_LENGTH, 30).

-include_lib("caterpillar.hrl").
-include_lib("caterpillar_storage.hrl").

-export([start_link/0, start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    storage='storage', 
    registered=false,
    work_id=0,
    work_id_file="./work_id",
    ceptaculum='ceptaculum@127.0.0.1'
}).

-define(CU, caterpillar_utils).


start_link() -> start_link([]).


start_link(Settings) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Settings, []).


init(Settings) ->
    error_logger:info_msg("initialized storage: ~p~n", [?GV(storage, Settings)]),
    filelib:ensure_dir(?GV(storage, Settings, ?DEFAULT_STORAGE)),
    {ok, Storage} = dets:open_file(storage, [{file, ?GV(storage, Settings, ?DEFAULT_STORAGE)}]),
    WorkIdFile = ?GV(work_id, Settings, ?DEFAULT_WORK_ID_FILE),
    WorkId = ?CU:read_work_id(WorkIdFile),
    Ceptaculum = ?GV(ceptaculum, Settings, 'ceptaculum@127.0.0.1'),
    async_register(),
    {ok, #state{
        storage=Storage,
        registered=false,
        work_id=WorkId,
        work_id_file=WorkIdFile,
        ceptaculum=Ceptaculum
    }}.


handle_call({storage_list_packages, Ident}, From, State) ->
    erlang:spawn(fun() -> 
        Reply = {ok, (catch dets:match(State#state.storage, {{Ident, '$1'}, '$2', ['$3'|'_']}))},
        gen_server:reply(From, Reply)
    end),
    {noreply, State};

handle_call({storage_list_package_builds, Ident, Name}, From, State) ->
    erlang:spawn(fun() -> 
        SelectPattern = {{Ident, '$1', {Name, '$2'}}, '$3', '$4', '$5', '$6', '_', '_'},
        Reply = {ok, (catch dets:match(State#state.storage, SelectPattern))},
        gen_server:reply(From, Reply)
    end),
    {noreply, State};

handle_call({storage_list_builds, Ident}, From, State) ->
    erlang:spawn(fun() -> 
        SelectPattern = [
            {{Ident, '$1', {'$2', '$3'}}, '$4', '$5', '$6', '_', '_', '_'},
            [{'>', '$1', State#state.work_id-?LIST_LENGTH}],
            ['$1', '$2', '$3', '$4', '$5', '$6']
        ],
        Reply = {ok, (catch dets:select(State#state.storage, SelectPattern))},
        gen_server:reply(From, Reply)
    end),
    {noreply, State};

handle_call({storage_build_info, Ident, Id}, From, State) ->
    erlang:spawn(fun() -> 
        SelectPattern = {{Ident, Id, {'$1', '$2'}}, '$3', '$4', '$5', '$6', '$7', '$8'},
        Reply = {ok, (catch dets:match(State#state.storage, SelectPattern))},
        gen_server:reply(From, Reply)
    end),
    {noreply, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({store_start_build, [IdentRecord, {Name, Branch}, WorkId, CommitHash]}, State=#state{storage=S}) ->
    Ident = {IdentRecord#ident.type, IdentRecord#ident.arch},
    ?CU:write_work_id(State#state.work_id_file, WorkId),
    case dets:lookup(S, {Ident, {Name, Branch}}) of
        [] ->
            dets:insert(S, {{Ident, {Name, Branch}}, "...", [WorkId]});
        [{_, Desc, Bids}] ->
            dets:insert(S, {{Ident, {Name, Branch}}, Desc, [WorkId|Bids]})
    end,
    dets:insert(S, {
        {Ident, WorkId, {Name, Branch}},
        <<"start">>,
        ?DTU:datetime_to_binary_string(calendar:universal_time()),
        <<"0000-00-00 00:00:00">>,
        CommitHash,
        <<"...">>,
        <<"...">>
    }),
    {noreply, State#state{work_id=WorkId}};

handle_cast({store_progress_build, [IdentRecord, {Name, Branch}, WorkId, Description]}, State=#state{storage=S}) ->
    Ident = {IdentRecord#ident.type, IdentRecord#ident.arch},
    case dets:lookup(S, {Ident, {Name, Branch}}) of
        [] ->
            dets:insert(S, {{Ident, {Name, Branch}}, Description, [WorkId]});
        [{_, _, Bids}] ->
            dets:insert(S, {{Ident, {Name, Branch}}, Description, Bids})
    end,
    case dets:lookup(S, {Ident, WorkId, {Name, Branch}}) of
        [{_, _, Start, End, CommitHash, _, _}] ->
            InsertData = {
                {Ident, WorkId, {Name, Branch}},
                <<"in_progress">>,
                Start,
                End,
                CommitHash,
                <<"...">>,
                <<"...">>
            },
            dets:insert(S, InsertData);
        _ ->
            pass
    end,
    {noreply, State};

handle_cast({store_error_build, [IdentRecord, {Name, Branch}, WorkId, BuildLog]}, State=#state{storage=S}) ->
    Ident = {IdentRecord#ident.type, IdentRecord#ident.arch},
    Link = case catch gen_server:call({ceptaculum, State#state.ceptaculum}, {put_file, [BuildLog, [{compress, true}]]}, 10000) of
        {[{_, {ok, L}}|_], _} ->
            L;
        Other ->
            error_logger:info_msg("failed to store build log: ~p~n", [Other]),
            <<"no_log">>
    end,
    case dets:lookup(S, {Ident, WorkId, {Name, Branch}}) of
        [{_, _, Start, _, CommitHash, _, _}|_] ->
            InsertData = {
                Ident,
                {WorkId, {Name, Branch}},
                <<"error">>,
                Start,
                ?DTU:datetime_to_binary_string(calendar:universal_time()),
                CommitHash,
                Link,
                <<"">>
            },
            dets:insert(S, InsertData);
        _ ->
            pass
    end,
    {noreply, State};

handle_cast({store_complete_build, [Ident,
            {Name, Branch},
            WorkId,
            Package,
            BuildLog
        ]}, State=#state{storage=S}) ->
    case dets:lookup(S, {Ident, WorkId, {Name, Branch}}) of
        [{_, _, Start, _, CommitHash, _, _}] ->
            InsertData = {
                {Ident, WorkId, {Name, Branch}},
                <<"success">>,
                Start,
                ?DTU:datetime_to_binary_string(calendar:universal_time()),
                CommitHash,
                BuildLog,
                Package
            },
            dets:insert(S, InsertData);
        _ ->
            pass
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({'DOWN', _, _, _, _}, State) ->
    async_register(),
    {noreply, State#state{registered=false}};

handle_info(async_register, State=#state{registered=false}) ->
    case catch caterpillar_event:register_service(storage) of
        {ok, Pid} -> 
            erlang:monitor(process, Pid),
            {noreply, State#state{registered=true}};
        _ ->
            async_register(),
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%-----------------


async_register() -> 
    async_register(1000).


async_register(Delay) ->
    erlang:send_after(Delay, self(), async_register).
