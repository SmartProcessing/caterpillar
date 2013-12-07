-module(caterpillar_storage).

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(DEFAULT_STORAGE, "/var/lib/smprc/caterpillar/storage").
-define(DEFAULT_WORK_ID_FILE, "/var/lib/smprc/caterpillar/storage_work_id").
-define(STORAGE_LENGTH, 500).
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
    rotate_interval,
    rotate_length,
    work_id_file="./work_id",
    file_path="/var/lib/smprc/caterpillar/storage/files",
    proc_state
}).

-define(CU, caterpillar_utils).


start_link() -> start_link([]).


start_link(Settings) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Settings, []).


init(Settings) ->
    error_logger:info_msg("initialized storage: ~p~n", [?GV(storage, Settings)]),
    filelib:ensure_dir(?GV(storage, Settings, ?DEFAULT_STORAGE)),
    {ok, Storage} = dets:open_file(storage, [{ram_file, true}, {file, ?GV(storage, Settings, ?DEFAULT_STORAGE)}]),
    WorkIdFile = ?GV(work_id, Settings, ?DEFAULT_WORK_ID_FILE),
    WorkId = ?CU:read_work_id(WorkIdFile),
    FileStorage = ?GV(file_path, Settings, "/var/lib/smprc/caterpillar/storage/files"),
    RotateTime = ?GV(rotate_interval, Settings, 10000),
    filelib:ensure_dir(FileStorage ++ "/empty"),
    async_register(),
    rotate(RotateTime),
    {ok, #state{
        storage=Storage,
        registered=false,
        work_id=WorkId,
        work_id_file=WorkIdFile,
        rotate_interval=RotateTime,
        rotate_length=?GV(rotate_length, Settings, ?STORAGE_LENGTH),
        file_path=FileStorage,
        proc_state=ets:new(proc_state, [])
    }}.


handle_call({storage, <<"packages">>, [Ident]}, From, State) ->
    {Pid, _Ref} = erlang:spawn_monitor(fun() ->
        MapFun = fun([{Name, Branch}, Description, LastBuild]=Test) ->
            SelectPattern = {{Ident, LastBuild, {Name, Branch}}, '$1', '_', '_', '_', '_', '_'},
            Status = case dets:match(State#state.storage, SelectPattern) of
                [[S]|_] ->
                    S;
                _Other ->
                    <<"unknown">>
            end,
            {<<Name/binary, <<"/">>/binary, Branch/binary>>,
                [
                    {<<"description">>, Description},
                    {<<"last_build_id">>, LastBuild},
                    {<<"last_build_state">>, Status}
                ]
            }
        end,
        Res = lists:map(MapFun, dets:match(State#state.storage, {{Ident, '$1'}, '$2', ['$3'|'_']})),
        gen_server:reply(From, Res)
    end),
    ets:insert(State#state.proc_state, {Pid, From}),
    {noreply, State};

handle_call({storage, <<"package">>, [Ident, Name]}, From, State) ->
    {Pid, _Ref} = erlang:spawn_monitor(fun() -> 
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
    ets:insert(State#state.proc_state, {Pid, From}),
    {noreply, State};

handle_call({storage, <<"builds">>, [Ident]}, From, State) ->
    {Pid, _Ref} = erlang:spawn_monitor(fun() -> 
        MapFun = fun([WorkId, Name, Branch, Status, Started, Finished]) ->
            Wid = list_to_binary(integer_to_list(WorkId)),
            {<<Wid/binary, <<"/">>/binary, Name/binary, <<"/">>/binary, Branch/binary>>,
                [
                    {<<"state">>, Status},
                    {<<"name">>, Name},
                    {<<"branch">>, Branch},
                    {<<"started">>, Started},
                    {<<"finished">>, Finished}
                ]
            }
        end,
        SelectPattern = [{
            {{Ident, '$1', {'$2', '$3'}}, '$4', '$5', '$6', '_', '_', '_'},
            [{'>=', '$1', State#state.work_id-?LIST_LENGTH}],
            [['$1', '$2', '$3', '$4', '$5', '$6']]
        }],
        Reply = lists:sort(fun
                ({K1S, _}, {K2S, _}) ->
                    [K1|_] = string:tokens(binary_to_list(K1S), "/"),
                    [K2|_] = string:tokens(binary_to_list(K2S), "/"),
                    list_to_integer(K1) > list_to_integer(K2)
                end,
            lists:map(MapFun, dets:select(State#state.storage, SelectPattern))),
        gen_server:reply(From, Reply)
    end),
    ets:insert(State#state.proc_state, {Pid, From}),
    {noreply, State};

handle_call({storage, <<"build">>, [Ident, IdBin]}, From, State) ->
    {Pid, _Ref} = erlang:spawn_monitor(fun() -> 
        Id = list_to_integer(binary_to_list(IdBin)),
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
    ets:insert(State#state.proc_state, {Pid, From}),
    {noreply, State};

handle_call({storage, <<"log">>, [Ident, IdBin, Name, Branch]}, From, State) ->
    Id = list_to_integer(binary_to_list(IdBin)),
    {Pid, _Ref} = erlang:spawn_monitor(fun() -> 
        SelectPattern = {{Ident, Id, {Name, Branch}}, '_', '_', '_', '_', '$1', '_'},
        [[Link]|_] = dets:match(State#state.storage, SelectPattern),
        Content = case file:read_file(filename:join([State#state.file_path, get_dir(Link), Link])) of
            {ok, C} ->
                C;
            _Other ->
                <<"...">>
        end,
        gen_server:reply(From, [{<<"log">>, Content}])
    end),
    ets:insert(State#state.proc_state, {Pid, From}),
    {noreply, State};

handle_call(Request, _From, State) ->
    error_logger:info_msg("storage request: ~p~n", [Request]),
    {reply, unknown, State}.


handle_cast({store_start_build, Msg = [Ident, {Name, Branch}, WorkId, CommitHash]}, State=#state{storage=S}) ->
    ?CU:write_work_id(State#state.work_id_file, WorkId),
    case dets:lookup(S, {Ident, {Name, Branch}}) of
        [] ->
            dets:insert(S, {{Ident, {Name, Branch}}, <<"...">>, [WorkId]});
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
    dets:sync(S),
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
    dets:sync(S),
    {noreply, State};

handle_cast({store_error_build, [Ident, {Name, Branch}, WorkId, Status, BuildLog]}, State=#state{storage=S, file_path=Path}) ->
    Link = v4str(v4()),
    Fname = filename:join([Path, get_dir(Link), Link]),
    filelib:ensure_dir(Fname),
    file:write_file(Fname, BuildLog),
    case dets:lookup(S, {Ident, WorkId, {Name, Branch}}) of
        [{_, _, Start, _, CommitHash, _, _}|_] ->
            InsertData = {
                {Ident, WorkId, {Name, Branch}},
                Status,
                Start,
                ?DTU:datetime_to_binary_string(calendar:universal_time()),
                CommitHash,
                Link,
                <<"...">>
            },
            dets:insert(S, InsertData);
        _ ->
            pass
    end,
    dets:sync(S),
    {noreply, State};

handle_cast({store_complete_build, [Ident,
            {Name, Branch},
            WorkId,
            Package,
            BuildLog
        ]}, State=#state{storage=S}) ->
    Link = v4str(v4()),
    Fname = filename:join([State#state.file_path, get_dir(Link), Link]),
    filelib:ensure_dir(Fname),
    file:write_file(Fname, BuildLog),
    case dets:lookup(S, {Ident, WorkId, {Name, Branch}}) of
        [{_, _, Start, _, CommitHash, _, _}] ->
            InsertData = {
                {Ident, WorkId, {Name, Branch}},
                <<"success">>,
                Start,
                ?DTU:datetime_to_binary_string(calendar:universal_time()),
                CommitHash,
                Link,
                Package
            },
            dets:insert(S, InsertData);
        _ ->
            pass
    end,
    dets:sync(S),
    {noreply, State};

handle_cast(Msg, State) ->
    error_logger:info_msg("unknown cast message to storage: ~p~n", [Msg]),
    {noreply, State}.


handle_info({'DOWN', Reference, _, Pid, normal}, State) ->
    catch ets:delete(State#state.proc_state, Pid),
    {noreply, State};
handle_info({'DOWN', _, _, Pid, _}, State) ->
    Registered = case ets:lookup(State#state.proc_state, Pid) of
        [{Pif, From}] ->
            gen_server:reply(From, <<"internal error">>),
            ets:delete(State#state.proc_state, Pid),
            State#state.registered;
        _Other ->
            async_register(),
            false
    end,
    {noreply, State#state{registered=Registered}};

handle_info(rotate, State=#state{storage=S, file_path=Path}) ->
    erlang:spawn(fun() -> 
        error_logger:info_msg("rotating storage~n"),
        MapFun = fun([WorkId, Name, Branch, Log, Ident]) ->
            catch file:delete(filename:join([Path, get_dir(Log), Log])),
            dets:delete(S, {Ident, WorkId, {Name, Branch}})
        end,
        SelectPattern = [{
            {{'$5', '$1', {'$2', '$3'}}, '_', '_', '_', '_', '$4', '_'},
            [{'<', '$1', State#state.work_id-State#state.rotate_length}],
            [['$1', '$2', '$3', '$4', '$5']]
        }],
        lists:map(MapFun, dets:select(State#state.storage, SelectPattern))
    end),
    rotate(State#state.rotate_interval),
    {noreply, State};

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

rotate(Time) ->
    erlang:send_after(Time, self(), rotate).

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
