-module(caterpillar_storage).

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(DEFAULT_STORAGE, "/var/lib/smprc/caterpillar/storage").
-define(DEFAULT_WORK_ID_FILE, "/var/lib/smprc/caterpillar/storage_work_id").
-define(DTU, smprc_datetime_utils).
-define(LIST_LENGTH, 30).
-define(V_PREFIX, 1048676).

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
    file_path="/var/lib/smprc/caterpillar/storage/files"
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
    FileStorage = ?GV(file_path, Settings, "/var/lib/smprc/caterpillar/storage/files"),
    filelib:ensure_dir(FileStorage ++ "/empty"),
    async_register(),
    {ok, #state{
        storage=Storage,
        registered=false,
        work_id=WorkId,
        work_id_file=WorkIdFile,
        file_path=FileStorage
    }}.


handle_call({storage, <<"packages">>, [Ident]}, From, State) ->
    erlang:spawn(fun() ->
        MapFun = fun([{Name, Branch}, Description, LastBuild]) ->
            SelectPattern = {{Ident, LastBuild, {Name, Branch}}, '$1', '_', '_', '_', '_', '_'},
            [[State]|_] = dets:match(State#state.storage, SelectPattern),
            {<<Name/binary, <<"/">>/binary, Branch/binary>>,
                [
                    {<<"description">>, Description},
                    {<<"last_build_id">>, LastBuild},
                    {<<"last_build_state">>, State}
                ]
            }
        end,
        Res = lists:map(MapFun, dets:match(State#state.storage, {{Ident, '$1'}, '$2', ['$3'|'_']})),
        gen_server:reply(From, Res)
    end),
    {noreply, State};

handle_call({storage, <<"package">>, [Ident, Name]}, From, State) ->
    erlang:spawn(fun() -> 
        MapFun = fun([WorkId, Branch, Status, Started, Finished, Hash]) ->
            Wid = list_to_binary(integer_to_list(WorkId)),
            {Wid,
                [
                    {<<"state">>, Status},
                    {<<"branch">>, Branch},
                    {<<"started">>, Started},
                    {<<"finished">>, Finished},
                    {<<"sha">>, Hash}
                ]
            }
        end,
        SelectPattern = {{Ident, '$1', {Name, '$2'}}, '$3', '$4', '$5', '$6', '_', '_'},
        gen_server:reply(From, lists:map(MapFun, dets:match(State#state.storage, SelectPattern)))
    end),
    {noreply, State};

handle_call({storage, <<"builds">>, [Ident]}, From, State) ->
    erlang:spawn(fun() -> 
        MapFun = fun([WorkId, Name, Branch, Status, Started, Finished]) ->
            Wid = list_to_binary(integer_to_list(WorkId)),
            {Wid,
                [
                    {<<"state">>, Status},
                    {<<"name">>, Name},
                    {<<"branch">>, Branch},
                    {<<"started">>, Started},
                    {<<"finished">>, Finished}
                ]
            }
        end,
        SelectPattern = [
            {{Ident, '$1', {'$2', '$3'}}, '$4', '$5', '$6', '_', '_', '_'},
            [{'>', '$1', State#state.work_id-?LIST_LENGTH}],
            ['$1', '$2', '$3', '$4', '$5', '$6']
        ],
        Reply = lists:map(MapFun, dets:select(State#state.storage, SelectPattern)),
        gen_server:reply(From, Reply)
    end),
    {noreply, State};

handle_call({storage, <<"build">>, [Ident, Id]}, From, State) ->
    erlang:spawn(fun() -> 
        MapFun = fun([Name, Branch, Status, Started, Finished, Hash, Log, Package]) ->
            {<<Name/binary, <<"/">>/binary, Branch/binary>>,
                [
                    {<<"state">>, Status},
                    {<<"started">>, Started},
                    {<<"finished">>, Finished},
                    {<<"sha">>, Hash},
                    {<<"package">>, Package}
                ]
            }
        end,
        SelectPattern = {{Ident, Id, {'$1', '$2'}}, '$3', '$4', '$5', '$6', '$7', '$8'},
        gen_server:reply(From, lists:map(MapFun, dets:match(State#state.storage, SelectPattern)))
    end),
    {noreply, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({store_start_build, [Ident, {Name, Branch}, WorkId, CommitHash]}, State=#state{storage=S}) ->
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

handle_cast({store_progress_build, [Ident, {Name, Branch}, WorkId, Description]}, State=#state{storage=S}) ->
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

handle_cast({store_error_build, [Ident, {Name, Branch}, WorkId, BuildLog]}, State=#state{storage=S, file_path=Path}) ->
    Link = v4str(v4()),
    file:write_file(filename:join([Path, get_dir(Link), Link]), BuildLog),
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
% Utils

async_register() -> 
    async_register(1000).


async_register(Delay) ->
    erlang:send_after(Delay, self(), async_register).

v4() ->
    v4(
        crypto:rand_uniform(1, round(math:pow(2, 48))) - 1, 
        crypto:rand_uniform(1, round(math:pow(2, 12))) - 1, 
        crypto:rand_uniform(1, round(math:pow(2, 32))) - 1, 
        crypto:rand_uniform(1, round(math:pow(2, 30))) - 1
    ).

v4(R1, R2, R3, R4) ->
    <<R1:48, 4:4, R2:12, 2:2, R3:32, R4: 30>>.

v4str(U) ->
    lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b", get_parts(U))).

get_parts(<<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>>) ->
    [TL, TM, THV, CSR, CSL, N].

get_dir(Link) ->
    <<DirInt:8, _R/binary>> = crypto:md5(Link),
    vessel_from_number(DirInt).

vessel_from_number(Int) when is_integer(Int) ->
    lists:flatten(io_lib:format("~.16B", [?V_PREFIX+Int])).
