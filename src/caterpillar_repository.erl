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
    DetsFile = ?GV(repository_db, Args, ?DETS),
    filelib:ensure_dir(DetsFile),
    Dets = case dets:open_file(DetsFile, [{access, read_write}]) of
        {ok, D} -> D;
        Error -> 
            error_logger:error_msg("caterpillar_repository dets initialization error: ~p~n", [Error]),
            throw({caterpillar_repository, {dets, Error}})
    end,
    check_repository_db_version(Dets),
    WorkIdFile = ?GV(work_id_file, Args, ?WORK_ID_FILE),
    filelib:ensure_dir(WorkIdFile),
    State = vcs_init(
        #state{
            work_id = caterpillar_utils:read_work_id(WorkIdFile),
            work_id_file = WorkIdFile,
            dets = Dets, %{{Package, Branch}, ArchiveName, ArchiveType, LastRevision, Tag, WorkId}
            repository_root = caterpillar_utils:ensure_dir(?GV(repository_root, Args, ?REPOSITORY_ROOT)),
            archive_root = caterpillar_utils:ensure_dir(?GV(archive_root, Args, ?ARCHIVE_ROOT)),
            notify_root = caterpillar_utils:ensure_dir(?GV(notify_root, Args, ?NOTIFY_ROOT)),
            scan_interval = ?GV(scan_interval, Args, ?SCAN_INTERVAL) * 1000,
            cleanup_interval = ?GV(cleanup_interval, Args, ?CLEANUP_INTERVAL) * 1000,
            cleanup_timer = clean_repository(0),
            registered = false,
            register_service_timer = register_service(0),
            scan_timer = scan_repository(0),
            vcs_plugin = ?GV(vcs_plugin, Args)
        },
        Args
    ),
    async_notify(),
    {ok, State}.


%async event for whole repository scan, checking every package and branch
handle_info(scan_repository, State) ->
    spawn(fun() -> scan_pipe(State) end),
    {noreply, scan_repository(State)};

%cleanup event
handle_info(clean_repository, State) ->
    spawn(fun() -> cleanup_pipe(State) end),
    {noreply, clean_repository(State)};

%async event for pushing saved notifications
handle_info(async_notify, State) ->
    spawn(fun() -> async_notify(State) end),
    async_notify(),
    {noreply, State};

%async event for registering in caterpillar_event
handle_info(register_service, #state{registered=Bool}=State) ->
    case Bool of 
        true -> {noreply, State};
        false -> {noreply, register_service(State)}
    end;

%caterpillar_event down
handle_info({'DOWN', _, _, _, _}, State) ->
    {noreply, register_service(State)};

handle_info(_Msg, State) ->
    {noreply, State}.


%cleaning removed packages, event generated while scan repository(scan_repository)
handle_cast({clean_packages, Notify, PackageName}, State) ->
    clean_packages(State, PackageName),
    spawn(fun() ->
        Event = [#archive{name=Name, branch=Branch} || {Name, Branch} <- PackageName],
        caterpillar_event:event({clean_packages, Event}),
        notify(State, Notify)
    end),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_call({repository_custom_command, Command, Args}, From, #state{vcs_plugin=VCP}=State) ->
    spawn(fun() -> 
        Response = (catch begin
            NewArgs = [State] ++ Args,
            apply(VCP, Command, NewArgs)
        end),
        gen_server:reply(From, Response)
    end),
    {noreply, State};

handle_call(rescan_repository, _From, State) ->
    scan_repository(0),
    {reply, ok, State};

handle_call({rescan_package, #package{}=Package}, _From, State) ->
    Pid = spawn(fun() -> scan_pipe(Package, State) end),
    {reply, {ok, Pid}, State};

handle_call({rebuild_package, #package{}=Package}, _From, State) ->
    Pid = spawn(fun() -> rebuild_package(Package, State) end),
    {reply, {ok, Pid}, State};

%copying archive to remote fd
handle_call({get_archive, #archive{name=Name, branch=Branch, fd=Fd}}, From, State) ->
    spawn(fun() ->
        ArchiveRoot = State#state.archive_root,
        Dets = State#state.dets,
        SelectPattern = [{
            {{'$1', '$2'}, '$3', '_', '_', '_', '_'},
            [{'andalso', {'==', '$1', Name}, {'==', '$2', Branch}}],
            ['$3']
        }],
        Reply = case catch dets:select(Dets, SelectPattern) of
            [ArchiveName] ->
                file:copy(caterpillar_utils:filename_join(ArchiveRoot, ArchiveName), Fd);
            BadReturn ->
                error_logger:info_msg(
                    "get_archive cant select archive for package ~p/~p with: ~p~n",
                    [Name, Branch, BadReturn]
                ),
                {error, get_archive}
        end,
        case Reply of
            {ok, _} -> gen_server:reply(From, ok);
            _ -> gen_server:reply(From, Reply)
        end
    end),
    {noreply, State};

%getting archives with work_id > WorkId
handle_call({get_archives, WorkId}, From, State) ->
    spawn(fun() ->
        Archives = (catch select_archives_by_work_id(State, WorkId)),
        gen_server:reply(From, {ok, {changes, State#state.work_id, Archives}})
    end),
    {noreply, State};

%getting all packages in local storage 
handle_call(get_packages, _From, #state{dets=D}=State) ->
    Packages = dets:select(D, [{{'$1', '_', '_', '_', '_', '_'}, [], ['$1']}]),
    {reply, Packages, State};

%event for pushing new archives and notify, generated in scan_repository
handle_call({changes, Changes}, _From, State) ->
    #state{work_id=WorkId, work_id_file=WorkIdFile, dets=Dets}=State,
    #changes{notify=Notify, packages=Packages, archives=Archives}=Changes,
    Packages = Changes#changes.packages,
    NewWorkId = WorkId + 1,
    InsertForeach = fun
        (#repository_package{name=Name, branch=Branch, archive_name=Archive, archive_type=ArchiveType, current_revno=Revno, tag=Tag}) ->
            dets:insert(Dets, {{Name, Branch}, Archive, ArchiveType, Revno, Tag, NewWorkId});
        (BadPackage) ->
            error_logger:error_mad("bad package at changes: ~p", [BadPackage])
    end,
    lists:foreach(InsertForeach, Packages),
    caterpillar_utils:write_work_id(WorkIdFile, NewWorkId),
    NewState = State#state{work_id=NewWorkId},
    spawn(fun() ->
        Archives = Changes#changes.archives,
        error_logger:info_msg("archives: ~p~n", [Archives]),
        case Archives of
            [] -> ok;
            _ -> caterpillar_event:event({changes, NewWorkId, Archives})
        end,
        NotifySubject = list_to_binary(io_lib:format("changes for build ~p", [NewWorkId])),
        notify(NewState, Notify#notify{subject=NotifySubject})
    end),
    {reply, ok, NewState};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(Msg, _From, State) ->
    error_logger:error_msg("bad message: ~p~n", [Msg]),
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
vcs_init(#state{vcs_plugin=VcsPlugin}=State, Args) ->
    catch begin
        case catch VcsPlugin:init_plugin(?GV(vcs_plugin_init, Args, [])) of
            {ok, VcsState} -> State#state{vcs_plugin=VcsPlugin, vcs_state=VcsState};
            Error -> {error, {vcs_init, Error}}
        end
    end.


-spec scan_repository(#state{}|non_neg_integer()) -> #state{}|reference().
scan_repository(#state{scan_interval=SI, scan_timer=ST}=State) ->
    catch erlang:cancel_timer(ST),
    State#state{scan_timer=scan_repository(SI)};
scan_repository(Delay) when is_integer(Delay), Delay >= 0 ->
    erlang:send_after(Delay, self(), scan_repository).


-spec clean_repository(#state{}|non_neg_integer()) -> #state{}|reference().
clean_repository(#state{cleanup_interval=CI, cleanup_timer=CT}=State) ->
    catch erlang:cancel_timer(CT),
    State#state{cleanup_timer=clean_repository(CI)};
clean_repository(Delay) when is_integer(Delay), Delay >= 0 ->
    erlang:send_after(Delay, self(), clean_repository).


-spec clean_packages(#state{}, [{ArchiveName::term(), Branch::term()}]) -> ok.
clean_packages(_State, []) -> ok;
clean_packages(#state{dets=Dets, archive_root=ArchiveRoot}=State, [{Name, Branch}|O]) ->
    file:delete(caterpillar_utils:filename_join(ArchiveRoot, caterpillar_utils:package_to_archive(Name, Branch))),
    dets:delete(Dets, {Name, Branch}),
    clean_packages(State, O).


-spec select_archives_by_work_id(#state{}, WorkId::pos_integer()) -> [#archive{}].
select_archives_by_work_id(#state{dets=Dets}, WorkId) ->
    Select = [{{'$1', '$2', '$3', '_', '$4', '$5'}, [{'<', WorkId, '$5'}], [['$1', '$2', '$3', '$4']]}],
    ArchiveList = dets:select(Dets, Select),
    MapFun = fun([{Name, Branch}, ArchiveName, ArchiveType, Tag]) ->
        #archive{name=Name, branch=Branch, archive_name=ArchiveName, archive_type=ArchiveType, tag=Tag}
    end,
    lists:map(MapFun, ArchiveList).


-spec async_notify() -> Timer::reference().
async_notify() -> erlang:send_after(5000, self(), async_notify).


%---------------


-spec async_notify(State::#state{}) -> no_return().
async_notify(#state{notify_root=NR}) ->
    case catch register(async_notify, self()) of
        true -> ok;
        _ ->
            error_logger:info_msg("async_notify already in process~n"),
            exit(normal)
    end,
    case file:list_dir(NR) of
        {ok, Files} -> 
            ForeachFun = fun(File) ->
                AbsFile = caterpillar_utils:filename_join(NR, File),
                Result = (catch begin 
                    {ok, Data} = file:read_file(AbsFile),
                    Msg = binary_to_term(Data),
                    ok = caterpillar_event:sync_event({notify, Msg}),
                    file:delete(AbsFile)
                end),
                error_logger:info_msg("async_notify result: ~p~n", [Result])
            end,
            lists:foreach(ForeachFun, Files);
        Error -> error_logger:info_msg("async_notify error while listing messages: ~p~n", [Error])
    end,
    ok.



-spec notify(#state{}, #notify{}) -> no_return().
notify(#state{notify_root=NR}, #notify{}=Notify) ->
    case catch caterpillar_event:sync_event({notify, Notify}) of
        ok -> ok;
        Error -> 
            {A, B, C} = os:timestamp(),
            Name = A * 1000000 * 1000000 + B * 1000000 + C,
            FileName = caterpillar_utils:filename_join(NR, integer_to_list(Name)),
            error_logger:error_msg(
                "failed to notify with error: ~p~nsaving packages dump to ~p~n",
                [Error, FileName]
            ),
            file:write_file(FileName, term_to_binary(Notify)) 
    end,
    ok.


%-----------


-spec check_repository_db_version(dets:tab()) -> ok.
check_repository_db_version(Dets) ->
    case dets:match(Dets, {version, '$1'}) of
        [[?REPOSITORY_DB_VERSION]] ->
            ok;
        Other ->
            error_logger:info_msg("check_repository_db_version failed: ~p~n", [Other]),
            ok = dets:delete_all_objects(Dets),
            dets:insert(Dets, {version, ?REPOSITORY_DB_VERSION})
    end,
    dets:sync(Dets),
    ok.


%-----------


-spec scan_pipe(#state{}) -> no_return().
scan_pipe(State) ->
    scan_pipe([], State).


scan_pipe(#package{name=Name, branch=Branch}, State) ->
    RepoPackage = [#repository_package{name=Name, branch=Branch}],
    scan_pipe(RepoPackage, State);

scan_pipe(Packages, State) ->
    FunList = [
        {scan_input, fun scan_input/2},
        {get_packages, fun get_packages/2},
        {get_branches, fun get_branches/2},
        {find_modified_packages, fun find_modified_packages/2},
        {export_archives, fun export_archives/2},
        {get_diff, fun get_diff/2},
        {get_changelog, fun get_changelog/2},
        {get_tag, fun get_tag/2},
        {build_changes, fun build_changes/2},
        {send_changes, fun send_changes/2}
    ],
    caterpillar_utils:pipe(FunList, Packages, State).


cleanup_pipe(State) ->
    FunList = [
        {get_packages, fun get_packages/2},
        {get_branches, fun get_branches/2},
        {cast_clean_packages, fun cast_clean_packages/2}
    ],
    caterpillar_utils:pipe(FunList, [], State).


-spec scan_input([#package{}], #state{}) -> {ok, [#repository_package{}]}|{error, Reason::term()}.
scan_input(Packages, _State) ->
    FoldFun = fun
        (#package{name=Name, branch=Branch}, Acc) -> [#repository_package{name=Name, branch=Branch}|Acc];
        (BadPackage, Acc) -> 
            error_logger:error_msg("scan_input bad package: ~p~n", [BadPackage]),
            Acc
    end,
    case lists:foldl(FoldFun, [], Packages) of
        [] -> 
            case catch register(scan_pipe_caterpillar_repository, self()) of
                true ->
                    error_logger:info_msg("scanning whole repository~n"),
                    {ok, []};
                _Err ->
                    error_logger:error_msg("whole repository scan already in process~n"),
                    {error, already_in_process}
            end;
        Result -> {ok, Result}
    end.


-spec get_packages([#repository_package{}], #state{}) -> {ok, [#repository_package{}]}|{error, Reason::term()}.
get_packages([], #state{repository_root=RepositoryRoot}=State) ->
    case caterpillar_utils:list_packages(RepositoryRoot) of
        {ok, []} -> 
            error_logger:info_msg("no packages in repository, stoping~n"),
            {stop, done};
        {ok, Packages} ->
            RepoPackages = lists:foldl(
                fun
                    (Name, Accum) when is_list(Name) -> [#repository_package{name=Name, branch='_'}|Accum];
                    (BadName, Accum) ->
                        error_logger:error_msg("get_packages bad repository package name: ~p~n", [BadName]),
                        Accum
                end,
                [],
                Packages
            ),
            get_packages(RepoPackages, State);
        {error, Reason} -> {error, {get_packages, Reason}};
        Other -> {error, {get_packages, {bad_result, Other}}}
    end;
get_packages(Packages, State) ->
    case get_packages(Packages, [], State) of
        {ok, []} -> {stop, done};
        {ok, ResultPackages} -> {ok, ResultPackages};
        {error, Reason} -> {error, {get_packages, Reason}};
        Other -> {error, {get_packages, {bad_result, Other}}}
    end.


get_packages([], Accum, _State) -> {ok, Accum};
get_packages([#repository_package{name=Name}=Package|Rest], Accum, State) when is_list(Name) ->
    #state{repository_root=RepositoryRoot, vcs_plugin=VCSPlugin, vcs_state=VCSState} = State,
    AbsolutePath = caterpillar_utils:filename_join(RepositoryRoot, Name),
    case catch VCSPlugin:is_repository(VCSState, AbsolutePath) of
        true -> 
            get_packages(Rest, [Package|Accum], State);
        false ->
            error_logger:info_msg("~p is not repository~n", [Package]),
            get_packages(Rest, Accum, State);
        Error ->
            {error, Error}
    end;
get_packages([BadPackage|_Rest], _Accum, _State) ->
    error_logger:error_msg("get_packages bad package: ~p~n", [BadPackage]),
    {error, {bad_package, BadPackage}}.


-spec get_branches([#repository_package{}], #state{}) -> {ok, [#repository_package{}]}.
get_branches(Packages, State) -> get_branches(Packages, [],  State).


get_branches([], Accum, _State) -> {ok, Accum};
get_branches([#repository_package{name=Name, branch='_'}=Package|Rest], Accum, State) -> %all branches for repo
    #state{repository_root=RepositoryRoot, vcs_plugin=VCSPlugin, vcs_state=VCSState} = State,
    AbsoluteName = caterpillar_utils:filename_join(RepositoryRoot, Name),
    NewAccum = case catch VCSPlugin:get_branches(VCSState, AbsoluteName) of
        {ok, []} -> 
            error_logger:info_msg("no branches in ~p~n", [Name]),
            Accum;
        {ok, Branches} -> 
            lists:append([#repository_package{name=Name, branch=Branch} || Branch <- Branches], Accum);
        Error -> 
            error_logger:error_msg("get_branches error: ~p~n on ~p~n", [Error, Package]),
            Accum
    end,
    get_branches(Rest, NewAccum, State);
get_branches([#repository_package{name=Name, branch=Branch}=Package|Rest], Accum, State) when is_list(Branch) ->
    #state{repository_root=RepositoryRoot, vcs_plugin=VCSPlugin, vcs_state=VCSState} = State,
    AbsoluteName = caterpillar_utils:filename_join(RepositoryRoot, Name),
    NewAccum = case catch VCSPlugin:is_branch(VCSState, AbsoluteName, Branch) of
        true -> [Package|Accum];
        false ->
            error_logger:info_msg("not branch: ~p/~p~n", [Name, Branch]),
            Accum;
        Error ->
            error_logger:error_msg("get_branches check branch error: at ~s/~s ~p~n", [Name, Branch, Error]),
            Accum
    end,
    get_branches(Rest, NewAccum, State);
get_branches([BadPackage|Rest], Accum, State) ->
    error_logger:error_msg("get_branches bad package: ~p~n", [BadPackage]),
    get_branches(Rest, Accum, State).


cast_clean_packages(Branches, #state{dets=Dets}) -> 
    DetsBranches = dets:select(Dets, [{{'$1', '_', '_', '_', '_', '_'}, [], ['$1']}]),
    case DetsBranches -- [{Name, Branch} || #repository_package{name=Name, branch=Branch} <- Branches] of
        [] -> ok;
        ToClean ->
            Body = list_to_binary(
                [io_lib:format("~s/~s~n", [Package, Name]) || {Package, Name} <- ToClean]
            ),
            Notify = #notify{subject = <<"some packages deleted">>, body = Body},
            gen_server:cast(?MODULE, {clean_packages, Notify, ToClean})
    end,
    {ok, Branches}.


find_modified_packages(Branches, State) ->
    find_modified_packages(Branches, [], State).


find_modified_packages([], [], _State) ->
    {error, {find_modified_packages, "no packages modified"}};

find_modified_packages([], Acc, _State) ->
    {ok, lists:reverse(Acc)};

find_modified_packages([Package|O], Accum, State) ->
    #state{vcs_plugin=VCSPlugin, vcs_state=VCSState, repository_root=RepositoryRoot}=State,
    #repository_package{name=PackageName, branch=PackageBranch} = Package,
    AbsPackage = caterpillar_utils:filename_join(RepositoryRoot, PackageName),
    SelectPattern = [{{{'$1', '$2'}, '_', '_', '$3', '_', '_'}, [{'==', '$1', PackageName}, {'==', '$2', PackageBranch}], ['$3']}],
    DetsResult = dets:select(State#state.dets, SelectPattern),
    DetsRevno = case DetsResult of
        [] -> none;
        [R] -> R
    end,
    NewAccum = case catch VCSPlugin:get_revno(VCSState, AbsPackage, PackageBranch) of
        {ok, Revno} when Revno /= DetsRevno ->
            [Package#repository_package{old_revno=DetsRevno, current_revno=Revno}|Accum];
        {ok, _} -> Accum;
        Error -> 
            error_logger:error_msg(
                "find_modified_packages error: ~p~n on ~p/~p~n",
                [Error, PackageName, PackageBranch]
            ),
            [Package#repository_package{
                old_revno=DetsRevno, status=error, failed_at=find_modified_packages, reason=Error
            }|Accum]
    end,
    find_modified_packages(O, NewAccum, State).


export_archives(Packages, State) -> 
    export_archives(Packages, [], State).


export_archives([], [], _State) ->
    {error, {export_archives, "nothing exported"}};

export_archives([], Accum, _State) ->
    {ok, lists:reverse(Accum)};

export_archives([#repository_package{status=error}=Package|O], Accum, State) ->
    export_archives(O, [Package|Accum], State);

export_archives([Package|O], Accum, #state{archive_root=ArchiveRoot, repository_root=RepoRoot}=State) ->
    #state{vcs_plugin=VCSPlugin, vcs_state=VCSState} = State,
    #repository_package{name=Name, branch=Branch, current_revno=Revno} = Package,
    RepoPath = caterpillar_utils:filename_join([RepoRoot, Name]),
    ArchiveName = caterpillar_utils:package_to_archive(Name, Branch),
    ArchivePath = caterpillar_utils:filename_join([ArchiveRoot, ArchiveName]),
    caterpillar_utils:ensure_dir(ArchiveRoot),
    NewAccum = case catch VCSPlugin:export_archive(VCSState, RepoPath, Branch, Revno, ArchivePath) of
        {ok, Type} -> [Package#repository_package{archive_type=Type, archive_name=ArchiveName}|Accum];
        {error, Reason} ->
            error_logger:error_msg(
                "export_archives error ~p~n at ~p/~p~n", [Reason, Name, Branch]
            ),
            [Package#repository_package{status=error, failed_at=export_archives, reason=Reason}|Accum];
        BadResponse ->
            error_logger:error_msg(
                "export_archive bad_return: ~p~n at ~p/~p~n", [BadResponse, Name, Branch]
            ),
            [Package#repository_package{status=error, failed_at=export_archives, reason=bad_return}|Accum]
    end,
    export_archives(O, NewAccum, State).



get_diff(Packages, State) ->
    get_diff(Packages, [], State).


get_diff([], Accum, _State) ->
    {ok, lists:reverse(Accum)};

get_diff([#repository_package{status=error}=Package|O], Accum, State) ->
    get_diff(O, [Package|Accum], State);

get_diff([Package|O], Accum, #state{repository_root=RepositoryRoot, vcs_plugin=VCSPlugin, vcs_state=VCSState}=State) ->
    #repository_package{name=PackageName, branch=PackageBranch, current_revno=CurrentRevno, old_revno=OldRevno} = Package,
    AbsName = caterpillar_utils:filename_join(RepositoryRoot, PackageName),
    Diff = case catch VCSPlugin:get_diff(VCSState, AbsName, PackageBranch, OldRevno, CurrentRevno) of
        {ok, D} when is_binary(D) -> D;
        Error ->
            error_logger:error_msg("get_diff error: ~p~n at ~p/~p~n", [Error, PackageName, PackageBranch]),
            <<"cant get diff">>
    end,
    get_diff(O, [Package#repository_package{diff=Diff}|Accum], State).



get_changelog(Packages, State) ->
    get_changelog(Packages, [], State).


get_changelog([], Accum, _State) ->
    {ok, lists:reverse(Accum)};

get_changelog([#repository_package{status=error}=Package|O], Accum, State) ->
    get_changelog(O, [Package|Accum], State);


get_changelog([Package|O], Accum, #state{repository_root=RepositoryRoot, vcs_plugin=VCSPlugin, vcs_state=VCSState}=State) ->
    #repository_package{name=PackageName, branch=PackageBranch, current_revno=CurrentRevno, old_revno=OldRevno}=Package,
    AbsPath = caterpillar_utils:filename_join(RepositoryRoot, PackageName),
    Changelog = case catch VCSPlugin:get_changelog(VCSState, AbsPath, PackageBranch, OldRevno, CurrentRevno) of
        {ok, C} when is_binary(C) -> C;
        Error ->
            error_logger:error_msg("get_changelog bad return: ~p~n at ~p/~p~n", [Error, PackageName, PackageBranch]),
            <<"cant get changelog">>
    end,
    get_changelog(O, [Package#repository_package{changelog=Changelog}|Accum], State).


get_tag(Packages, State) ->
    get_tag(Packages, [], State).


get_tag([], Accum, _State) ->
    {ok, lists:reverse(Accum)};
get_tag([#repository_package{status=error}=Package|O], Accum, State) ->
    get_tag(O, [Package|Accum], State);
get_tag([Package|O], Accum, #state{vcs_plugin=VcsP, vcs_state=VcsS}=State) ->
    RR = State#state.repository_root,
    Name = Package#repository_package.name,
    Branch = Package#repository_package.branch,
    Revno = Package#repository_package.current_revno,
    case catch VcsP:get_tag(VcsS, caterpillar_utils:filename_join(RR, Name), Branch, Revno) of
        {ok, T} -> 
            get_tag(O, [Package#repository_package{tag=T}|Accum], State);
        Error ->
            error_logger:error_msg("failed to get tag at ~s/~s with ~p~n", [Name, Package, Error]),
            get_tag(O, [Package|Accum], State)
    end.



build_changes(Packages, _State) ->
    Notify = #notify{subject = <<>>, body = <<>>},
    case catch build_changes(Packages, Notify, []) of
        {ok, {NewNotify, Archives}} ->
            NewPackages = [Package#repository_package{diff= <<>>, changelog= <<>>} || Package <- Packages],
            {ok, #changes{notify=NewNotify, archives=Archives, packages=NewPackages}};
        Err ->
            error_logger:error_msg("build_changes error: ~p~n", [Err]),
            {error, build_changes}
    end.


build_changes([], Notify, Archives) ->
    {ok, {Notify, lists:reverse(Archives)}};

build_changes([Package|O], Notify, ArchiveAccum) ->
    Name = Package#repository_package.name,
    Branch = Package#repository_package.branch,
    {PackageBody, NewArchiveAccum} = case Package#repository_package.status of
        ok ->
            Archive = #archive{
                name=Name, branch=Branch,
                archive_name=Package#repository_package.archive_name,
                archive_type=Package#repository_package.archive_type,
                tag=Package#repository_package.tag
            },
            {DiffLength, Diff} = limit_output(Package#repository_package.diff, 10240),
            {_, ChangeLog} = limit_output(Package#repository_package.changelog, 10240),
            Body = list_to_binary(
                io_lib:format(
                    "~s/~s~n~s~nDiff contains ~p bytes~n~s~n",
                    [Name, Branch, ChangeLog, DiffLength, Diff]
                )
            ),
            {Body,  [Archive|ArchiveAccum]};
        error ->
            FailedAt = Package#repository_package.failed_at,
            Reason = Package#repository_package.reason,
            Body = list_to_binary(
                io_lib:format(
                    "~s/~s failed at ~p~n~p~n",
                    [Name, Branch, FailedAt, Reason]
                )
            ),
            {Body, ArchiveAccum}
    end,
    OldBody = Notify#notify.body,
    NewBody = <<OldBody/binary, $\n, $\n, PackageBody/binary>>,
    build_changes(O, Notify#notify{body=NewBody}, NewArchiveAccum).


send_changes(Result, _State) ->
    gen_server:call(?MODULE, {changes, Result}, infinity),
    {ok, done}.


-spec limit_output(binary(), non_neg_integer()) -> {non_neg_integer(), binary()}.
limit_output(Bin, Size) when is_binary(Bin), is_integer(Size), Size > 0 ->
    case size(Bin) of
        S when S =< Size -> {S, Bin};
        S -> {S, << (binary_part(Bin, {0, Size}))/binary, "..." >>}
    end.



%----------


-spec rebuild_package(#package{}, #state{}) -> ok.
rebuild_package(#package{name=Name, branch=Branch}, #state{dets=Dets}) ->
    case catch dets:lookup(Dets, {Name, Branch}) of
        [] -> error_logger:error_msg("no ~p/~p for rebuild~n", [Name, Branch]);
        [{{Name, Branch}, ArchiveName, ArchiveType, LastRevision, Tag, _WorkId}] ->
            Package = #repository_package{
                name=Name, branch=Branch, tag=Tag, current_revno=LastRevision,
                archive_name=ArchiveName, archive_type=ArchiveType, status=ok
            },
            Archive = #archive{
                name=Name, branch=Branch, archive_name=ArchiveName, tag=Tag,
                archive_type=ArchiveType
            },
            Body = io_lib:format("rebuild request for ~s/~s~n", [Name, Branch]),
            Notify = #notify{body = list_to_binary(Body)},
            Changes = #changes{
                notify = Notify,
                archives = [Archive],
                packages = [Package]
            },
            gen_server:call(?MODULE, {changes, Changes}, infinity);
        Error ->
            error_logger:error_msg("rebuild_package error: ~p~n", [Error])
    end,
    ok.
