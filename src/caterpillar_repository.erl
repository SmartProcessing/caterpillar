-module(caterpillar_repository).

-behaviour(gen_server).

-include_lib("caterpillar.hrl").
-include_lib("caterpillar_repository_internal.hrl").

-export([start_link/1, stop/0]).
-export([init/1, handle_info/2, handle_cast/2, handle_call/3, terminate/2, code_change/3]).



start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).


stop() -> 
    gen_server:call(?MODULE, stop, infinity).


init(Args) ->
    Dets = case dets:open_file(?GV(repository_db, Args, ?DETS), [{access, read_write}]) of
        {ok, D} -> D;
        Error -> 
            error_logger:error_msg("caterpillar_repository dets initialization error: ~p~n", [Error]),
            throw({caterpillar_repository, {dets, Error}})
    end,
    WorkIdFile = caterpillar_utils:ensure_dir(?GV(work_id_file, Args, ?WORK_ID_FILE)),
    State = vcs_init(
        #state{
            work_id = caterpillar_utils:read_work_id(WorkIdFile),
            work_id_file = WorkIdFile,
            dets = Dets, %{{Package, Branch}, ArchiveName, LastRevision, WorkId}
            repository_root = caterpillar_utils:ensure_dir(?GV(repository_root, Args, ?REPOSITORY_ROOT)),
            export_root = caterpillar_utils:ensure_dir(?GV(export_root, Args, ?EXPORT_ROOT)),
            archive_root = caterpillar_utils:ensure_dir(?GV(archive_root, Args, ?ARCHIVE_ROOT)),
            notify_root = caterpillar_utils:ensure_dir(?GV(notify_root, Args, ?NOTIFY_ROOT)),
            scan_interval = ?GV(scan_interval, Args, ?SCAN_INTERVAL) * 1000,
            registered = false,
            register_service_timer = register_service(0),
            scan_timer = scan_repository(0)
        },
        Args
    ),
    {ok, State}.



handle_info(scan_repository, State) ->
    spawn(fun() ->
        Self = self(),
        case catch register(scan_pipe_caterpillar_repository, Self) of
            true -> ok;
            _Err ->
                error_logger:error_msg("scan_pipe already in process~n"),
                exit(normal)
        end,
        error_logger:info_msg("scan pipe started at ~p~n", [Self]),
        case catch scan_pipe(State) of
            {ok, Packages} when Packages /= [] ->
                gen_server:call(?MODULE, {new_packages, Packages}, infinity);
            Error ->
                error_logger:error_msg("scan pipe failed with: ~p~n", [Error])
        end
    end),
    {noreply, scan_repository(State)};

handle_info(register_service, #state{registered=Bool}=State) ->
    case Bool of 
        true -> {noreply, State};
        false -> {noreply, register_service(State)}
    end;

handle_info({'DOWN', _, _, _, _}, State) ->
    {noreply, register_service(State)};

handle_info(_Msg, State) ->
    {noreply, State}.


handle_cast({clean_packages, Packages}, State) ->
    clean_packages(State, Packages),
    spawn(fun() -> notify(clean_packages, State, Packages) end),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.


handle_call(get_packages, _From, #state{dets=D}=State) ->
    Packages = dets:select(D, [{{'$1', '_', '_', '_'}, [], ['$1']}]),
    {reply, Packages, State};

handle_call({new_packages, Packages}, _From, #state{dets=D}=State) ->
    NewWorkId = State#state.work_id + 1,
    WorkIdFile = State#state.work_id_file,
    [
        dets:insert(D, {{Name, Branch}, Archive, Revno, NewWorkId}) ||
        #package{name=Name, branch=Branch, archive=Archive, current_revno=Revno} <- Packages
    ],
    caterpillar_utils:write_work_id(WorkIdFile, NewWorkId),
    NewState = State#state{work_id=NewWorkId},
    spawn(fun() -> notify(new_packages, NewState, Packages) end),
    {reply, ok, NewState};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Msg, _From, State) ->
    {reply, {error, bad_msg}, State}.



terminate(Reason, #state{vcs_plugin=VCSPlugin, vcs_state=VCSState}) ->
    error_logger:info_msg("terminating ~p: ~p~n", [VCSPlugin, VCSPlugin:terminate_plugin(VCSState)]),
    error_logger:info_msg("caterpillar_repository down with reason ~p~n", [Reason]).


code_change(_Old, State, _Extra) ->
    {ok, State}.



%-------


-spec register_service(#state{}) -> NewState :: #state{}.
register_service(#state{registered=false, register_service_timer=RST}=State) ->
    catch erlang:cancel_timer(RST),
    case catch caterpillar_event:register_service(repository) of
        {ok, Pid} ->
            erlang:monitor(process, Pid),
            State#state{register_service_timer=undefined};
        Error ->
            error_logger:error_msg("failed to register in event service with error: ~p~n", [Error]),
            Ref = register_service(5000),
            State#state{register_service_timer=Ref}
    end;
register_service(Delay) ->
    erlang:send_after(Delay, self(), register_service).


-spec vcs_init(State::#state{}, Args::proplists:property()) -> NewState::#state{}.

vcs_init(State, Args) ->
    case catch vcs_init_(State, Args) of
        #state{}=NewState -> NewState;
        Error ->
            error_logger:error_msg("vcs_init failed with: ~p~n", [Error]),
            exit({vcs_init, failed})
    end.


vcs_init_(State, Args) ->
    VcsPlugin = ?GV(vcs_plugin, Args),
    case VcsPlugin:init_plugin(?GV(vcs_plugin_init, Args, [])) of
        {ok, VcsState} ->
            State#state{vcs_plugin=VcsPlugin, vcs_state=VcsState};
        Error -> Error
    end.


-spec scan_repository(#state{}|non_neg_integer()) -> #state{}|reference().

scan_repository(#state{scan_interval=SI, scan_timer=ST}=State) ->
    catch erlang:cancel_timer(ST),
    State#state{scan_timer=scan_repository(SI)};
scan_repository(Delay) when is_integer(Delay), Delay >= 0 ->
    erlang:send_after(Delay, self(), scan_repository).


clean_packages(_State, []) ->
    ok;
clean_packages(#state{dets=D, export_root=ER, archive_root=AR}=State, [Package|O]) ->
    Name = Package#package.name,
    Branch = Package#package.branch,
    AbsEr = filename:join(ER, Name),
    file:delete(filename:join(AR, caterpillar_utils:package_to_archive(Name, Branch))),
    dets:delete(D, {Name, Branch}),
    caterpillar_utils:del_dir(filename:join(AbsEr, Branch)),
    case catch caterpillar_utils:list_packages(AbsEr) of
        {ok, []} -> caterpillar_utils:del_dir(AbsEr);
        _ -> ok
    end,
    clean_packages(State, O).


%---------------

-spec notify(NotifyType :: atom(), #state{}, [#package{}]) -> no_return().
notify(Type, #state{notify_root=NR, work_id=Wid}, Packages) ->
    case catch caterpillar_event:sync_event({notify, {Type, Packages}}) of
        {ok, done} ->
            {ok, done};
        Error -> 
            FileName = filename:join(NR, integer_to_list(Wid)),
            error_logger:error_msg(
                "failed to notify with error: ~p~nsaving packages dump to ~p~n",
                [Error, FileName]
            ),
            file:write_file(FileName, term_to_binary({Type, Wid, Packages})) 
    end.



%-----------


scan_pipe(State) ->
    FunList = [
        {get_packages, fun get_packages/2},
        {get_brances, fun get_branches/2},
        {cast_clean_packages, fun cast_clean_packages/2},
        {find_modified_packages, fun find_modified_packages/2},
        {export_packages, fun export_packages/2},
        {archive_packages, fun archive_packages/2},
        {get_diff, fun get_diff/2},
        {get_changelog, fun get_changelog/2}
    ],
    caterpillar_utils:pipe(FunList, none, State).


get_packages(_, #state{repository_root=RR, vcs_plugin=VCSPlugin, vcs_state=VCSState}) ->
    case caterpillar_utils:list_packages(RR) of
        {ok, []} -> {error, {get_packages, "nothing in repository"}};
        {ok, Packages} ->
            FoldFun = fun(Package, Accum) ->
                case VCSPlugin:is_repository(VCSState, filename:join(RR, Package)) of
                    true -> [#package{name=Package} | Accum];
                    false ->
                        error_logger:info_msg("~p is not repository~n", [Package]),
                        Accum
                end 
            end,
            case catch lists:foldl(FoldFun, [], Packages) of
                [] -> {error, {get_packages, "no repositories available"}};
                Repos when is_list(Repos) -> {ok, lists:reverse(Repos)};
                Error -> {error, {get_packages, {plugin_bad_return, Error}}}
            end;
        Error -> {error, {get_packages, Error}}
    end.


get_branches(Packages, State) ->
    get_branches(Packages, [],  State).


get_branches([], [], _State) ->
    {error, {get_branches, "no branches in repositories"}};

get_branches([], Branches, _State) ->
    {ok, lists:sort(Branches)};

get_branches([Package|O], Accum, #state{repository_root=RR, vcs_plugin=VCSPlugin, vcs_state=VCSState}=State) ->
    AbsPackage = filename:join(RR, Package#package.name),
    NewAccum = case VCSPlugin:get_branches(VCSState, AbsPackage) of
        {ok, []} -> 
            error_logger:info_msg("no branches in ~p~n", [Package]),
            Accum;
        {ok, RawBranches} -> 
            FoldFun = fun(Branch, Acc) ->
                case catch VCSPlugin:is_branch(VCSState, AbsPackage, Branch) of
                    true ->
                        [Package#package{branch=Branch}|Acc];
                    false ->
                        error_logger:info_msg("~p/~p not a branch~n", [Package, Branch]), 
                        Acc;
                    Err ->
                        error_logger:error_msg(
                            "get_branches error: ~p~n on ~p/~p~n",
                            [Err, Package, Branch]
                        ),
                        Acc
                end
            end,
            case catch lists:foldl(FoldFun, Accum, RawBranches) of
                Branches when is_list(Branches) ->
                    Branches;
                Error ->
                    error_logger:error_msg("get_branches fold error: ~p~n on ~p~n", [Error, Package]),
                    Accum
            end;
        Error -> 
            error_logger:error_msg("get_branches error: ~p~n on ~p~n", [Error, Package]),
            Accum
    end,
    get_branches(O, NewAccum, State).


cast_clean_packages(Branches, #state{dets=Dets}) -> 
    DetsBranches = dets:select(Dets, [{{'$1', '_', '_', '_'}, [], ['$1']}]),
    case DetsBranches -- [{Name, Branch} || #package{name=Name, branch=Branch} <- Branches] of
        [] -> ok;
        ToClean ->
            gen_server:cast(
                ?MODULE,
                {clean_packages, [#package{name=Name, branch=Branch} || {Name, Branch} <- ToClean]}
            )
    end,
    {ok, Branches}.


find_modified_packages(Branches, State) ->
    find_modified_packages(Branches, [], State).


find_modified_packages([], [], _State) ->
    {error, {find_modified_packages, "no packages modified"}};

find_modified_packages([], Acc, _State) ->
    {ok, lists:reverse(Acc)};

find_modified_packages([Package|O], Accum, State) ->
    VCSPlugin = State#state.vcs_plugin,
    VCSState = State#state.vcs_state,
    RR = State#state.repository_root,
    PackageBranch = Package#package.branch,
    PackageName = Package#package.name,
    AbsPackage = filename:join(RR, PackageName),
    DetsResult = dets:select(
        State#state.dets,
        [{{
            {'$1', '$2'}, '_', '$3', '_'},
            [{'==', '$1', PackageName},
            {'==', '$2', PackageBranch}], ['$3']
        }]
    ),
    DetsRevno = case DetsResult of
        [] -> none;
        [R] -> R
    end,
    NewAccum = case catch VCSPlugin:get_revno(VCSState, AbsPackage, PackageBranch) of
        {ok, Revno} when Revno /= DetsRevno ->
            [Package#package{old_revno=DetsRevno, current_revno=Revno}|Accum];
        {ok, _} -> Accum;
        Error -> 
            error_logger:error_msg(
                "find_modified_packages error: ~p~n on ~p/~p~n",
                [Error, PackageName, PackageBranch]
            ),
            [Package#package{
                old_revno=DetsRevno, status=error, failed_at=find_modified_packages, reason=Error
            }|Accum]
    end,
    find_modified_packages(O, NewAccum, State).


export_packages(Packages, State) -> 
    export_packages(Packages, [], State).


export_packages([], [], _State) ->
    {error, {export_packages, "nothing exported"}};

export_packages([], Accum, _State) ->
    {ok, lists:reverse(Accum)};

export_packages([#package{status=error}=Package|O], Accum, State) ->
    export_packages(O, [Package|Accum], State);

export_packages([Package|O], Accum, #state{export_root=ER, repository_root=RR}=State) ->
    VCSPlugin = State#state.vcs_plugin,
    VCSState = State#state.vcs_state,
    PackageName = Package#package.name,
    PackageBranch = Package#package.branch,
    AbsExport = filename:join([ER, PackageName, PackageBranch]),
    AbsPackage = filename:join(RR, PackageName),
    caterpillar_utils:del_dir(AbsExport),
    caterpillar_utils:ensure_dir(AbsExport),
    NewAccum = case catch VCSPlugin:export(VCSState, AbsPackage, PackageBranch, AbsExport) of
        ok -> [Package|Accum];
        Error ->
            error_logger:error_msg(
                "export_packages error ~p~n at ~p/~p~n", [Error, PackageName, PackageBranch]
            ),
            [Package#package{status=error, failed_at=export_packages, reason=Error}|Accum]
    end,
    export_packages(O, NewAccum, State).


archive_packages(Packages, State) -> 
    archive_packages(Packages, [], State).


archive_packages([], [], _State) ->
    {error, {archive_packages, "nothing archived"}};

archive_packages([], Accum, _State) ->
    {ok, lists:reverse(Accum)};

archive_packages([#package{status=error}=Package|O], Accum, State) ->
    archive_packages(O, [Package|Accum], State);

archive_packages([Package|O], Accum, #state{export_root=ER, archive_root=AR}=State) ->
    PackageName = Package#package.name,
    PackageBranch = Package#package.branch,
    ExportPath = filename:join([ER, PackageName, PackageBranch]),
    ArchiveName = caterpillar_utils:package_to_archive(PackageName, PackageBranch),
    Archive = filename:join(AR, ArchiveName),
    Result = (catch begin
        Tar = case erl_tar:open(Archive, [write, compressed]) of
            {ok, T} -> T;
            ErrT -> throw(ErrT)
        end,
        ForeachAddFun = fun(File) -> 
            AbsFile = filename:join(ExportPath, File),
            case erl_tar:add(Tar, AbsFile, File, []) of
                ok -> ok;
                ErrAd -> throw(ErrAd)
            end
        end,
        {ok, Listing} = file:list_dir(ExportPath),
        lists:foreach(ForeachAddFun, Listing),
        erl_tar:close(Tar),
        ok
    end),
    NewAccum = case Result of
        ok -> [Package#package{archive=ArchiveName}|Accum];
        Error ->
            error_logger:error_msg(
                "archive_packages error: ~p~n at ~p/~p~n",
                [Error, PackageName, PackageBranch]
            ),
            [Package#package{status=error, failed_at=archive_packages, reason=Error}|Accum]
    end,
    archive_packages(O, NewAccum, State).


get_diff(Packages, State) ->
    get_diff(Packages, [], State).


get_diff([], Accum, _State) ->
    {ok, lists:reverse(Accum)};

get_diff([#package{status=error}=Package|O], Accum, State) ->
    get_diff(O, [Package|Accum], State);

get_diff([Package|O], Accum, #state{repository_root=RR, vcs_plugin=VCSPlugin, vcs_state=VCSState}=State) ->
    Name = Package#package.name,
    Branch = Package#package.branch,
    CurrentRevno = Package#package.current_revno,
    OldRevno = Package#package.old_revno,
    Diff = case catch VCSPlugin:get_diff(VCSState, filename:join(RR, Name), Branch, OldRevno, CurrentRevno) of
        {ok, D} when is_binary(D) -> D;
        Error ->
            error_logger:error_msg("get_diff error: ~p~n at ~p/~p~n", [Error, Name, Branch]),
            <<"cant get diff">>
    end,
    get_diff(O, [Package#package{diff=Diff}|Accum], State).



get_changelog(Packages, State) ->
    get_changelog(Packages, [], State).


get_changelog([], Accum, _State) ->
    {ok, lists:reverse(Accum)};

get_changelog([#package{status=error}=Package|O], Accum, State) ->
    get_changelog(O, [Package|Accum], State);


get_changelog([Package|O], Accum, #state{repository_root=RR, vcs_plugin=VCSPlugin, vcs_state=VCSState}=State) ->
    Name = Package#package.name,
    Branch = Package#package.branch,
    CurrentRevno = Package#package.current_revno,
    OldRevno = Package#package.old_revno,
    Changelog = case catch VCSPlugin:get_changelog(VCSState, filename:join(RR, Name), Branch, OldRevno, CurrentRevno) of
        {ok, C} when is_binary(C) -> C;
        Error ->
            error_logger:error_msg("get_changelog bad return: ~p~n at ~p/~p~n", [Error, Name, Branch]),
            <<"cant get changelog">>
    end,
    get_changelog(O, [Package#package{changelog=Changelog}|Accum], State).

