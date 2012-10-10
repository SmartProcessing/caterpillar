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
    WorkIdFile = ?GV(work_id_file, Args, ?WORK_ID_FILE),
    filelib:ensure_dir(WorkIdFile),
    State = vcs_init(
        #state{
            work_id = caterpillar_utils:read_work_id(WorkIdFile),
            work_id_file = WorkIdFile,
            dets = Dets, %{{Package, Branch}, ArchiveName, LastRevision, Tag, WorkId}
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
    async_notify(),
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
            {ok, #scan_pipe_result{}=Result} ->
                gen_server:call(?MODULE, {changes, Result}, infinity);
            Error ->
                error_logger:error_msg("scan pipe failed with: ~p~n", [Error])
        end
    end),
    {noreply, scan_repository(State)};

handle_info(async_notify, State) ->
    spawn(fun() -> async_notify(State) end),
    async_notify(),
    {noreply, State};

handle_info(register_service, #state{registered=Bool}=State) ->
    case Bool of 
        true -> {noreply, State};
        false -> {noreply, register_service(State)}
    end;

handle_info({'DOWN', _, _, _, _}, State) ->
    {noreply, register_service(State)};

handle_info(_Msg, State) ->
    {noreply, State}.


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


handle_call(get_packages, _From, #state{dets=D}=State) ->
    Packages = dets:select(D, [{{'$1', '_', '_', '_', '_'}, [], ['$1']}]),
    {reply, Packages, State};

handle_call({changes, ScanPipeResult}, _From, #state{dets=D}=State) ->
    NewWorkId = State#state.work_id + 1,
    WorkIdFile = State#state.work_id_file,
    Notify = ScanPipeResult#scan_pipe_result.notify,
    Packages = ScanPipeResult#scan_pipe_result.packages,
    [
        dets:insert(D, {{Name, Branch}, Archive, Revno, Tag, NewWorkId}) ||
        #package{name=Name, branch=Branch, archive_name=Archive, current_revno=Revno, tag=Tag} <- Packages
    ],
    caterpillar_utils:write_work_id(WorkIdFile, NewWorkId),
    NewState = State#state{work_id=NewWorkId},
    spawn(fun() ->
        Archives = ScanPipeResult#scan_pipe_result.archives,
        case Archives of
            [] -> ok;
            _ -> caterpillar_event:event({changes, NewWorkId, Archives})
        end,
        notify(
            NewState,
            Notify#notify{
                subject= list_to_binary(
                    io_lib:format("changes for build ~p", [NewWorkId])
                )
            }
        )
    end),
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
clean_packages(#state{dets=D, export_root=ER, archive_root=AR}=State, [{Name, Branch}|O]) ->
    AbsEr = filename:join(ER, Name),
    file:delete(filename:join(AR, caterpillar_utils:package_to_archive(Name, Branch))),
    dets:delete(D, {Name, Branch}),
    caterpillar_utils:del_dir(filename:join(AbsEr, Branch)),
    case catch caterpillar_utils:list_packages(AbsEr) of
        {ok, []} -> caterpillar_utils:del_dir(AbsEr);
        _ -> ok
    end,
    clean_packages(State, O).


async_notify() ->
    erlang:send_after(5000, self(), async_notify).


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
                AbsFile = filename:join(NR, File),
                case file:read_file(AbsFile) of
                    {ok, Data} ->
                        Msg = binary_to_term(Data),
                        case catch caterpillar_event:sync_event({notify, Msg}) of
                            ok -> ok = file:delete(AbsFile);
                            Error -> 
                                error_logger:info_msg(
                                    "async_notify failed to send sync_event with: ~p~n",
                                    [Error]
                                )
                        end;
                    Error ->
                        error_logger:info_msg("failed to async_notify with error: ~p~n", [Error])
                end
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
            FileName = filename:join(NR, integer_to_list(Name)),
            error_logger:error_msg(
                "failed to notify with error: ~p~nsaving packages dump to ~p~n",
                [Error, FileName]
            ),
            file:write_file(FileName, term_to_binary(Notify)) 
    end,
    ok.

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
        {get_changelog, fun get_changelog/2},
        {get_tag, fun get_tag/2},
        {build_result, fun build_result/2}
    ],
    caterpillar_utils:pipe(FunList, none, State).


get_packages(_, #state{repository_root=RR, vcs_plugin=VCSPlugin, vcs_state=VCSState}) ->
    case caterpillar_utils:list_packages(RR) of
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
                Repos when is_list(Repos) -> {ok, lists:reverse(Repos)};
                Error -> {error, {get_packages, {plugin_bad_return, Error}}}
            end;
        Error -> {error, {get_packages, Error}}
    end.


get_branches(Packages, State) ->
    get_branches(Packages, [],  State).


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
    DetsBranches = dets:select(Dets, [{{'$1', '_', '_', '_', '_'}, [], ['$1']}]),
    case DetsBranches -- [{Name, Branch} || #package{name=Name, branch=Branch} <- Branches] of
        [] -> ok;
        ToClean ->
            Body = list_to_binary(
                string:join(
                    [io_lib:format("~s/~s", [Package, Name]) || {Package, Name} <- ToClean],
                    $\n
                )
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
    VCSPlugin = State#state.vcs_plugin,
    VCSState = State#state.vcs_state,
    RR = State#state.repository_root,
    PackageBranch = Package#package.branch,
    PackageName = Package#package.name,
    AbsPackage = filename:join(RR, PackageName),
    DetsResult = dets:select(
        State#state.dets,
        [{{
            {'$1', '$2'}, '_', '$3', '_', '_'},
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
    Name = Package#package.name,
    Branch = Package#package.branch,
    Revno = Package#package.current_revno,
    AbsExport = filename:join([ER, Name, Branch]),
    AbsPackage = filename:join(RR, Name),
    caterpillar_utils:del_dir(AbsExport),
    caterpillar_utils:ensure_dir(AbsExport),
    NewAccum = case catch VCSPlugin:export(VCSState, AbsPackage, Branch, Revno, AbsExport) of
        ok -> [Package|Accum];
        Error ->
            error_logger:error_msg(
                "export_packages error ~p~n at ~p/~p~n", [Error, Name, Branch]
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
        ok -> [Package#package{archive_name=ArchiveName}|Accum];
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
    AbsPath = filename:join(RR, Name),
    Changelog = case catch VCSPlugin:get_changelog(VCSState, AbsPath, Branch, OldRevno, CurrentRevno) of
        {ok, C} when is_binary(C) -> C;
        Error ->
            error_logger:error_msg("get_changelog bad return: ~p~n at ~p/~p~n", [Error, Name, Branch]),
            <<"cant get changelog">>
    end,
    get_changelog(O, [Package#package{changelog=Changelog}|Accum], State).


get_tag(Packages, State) ->
    get_tag(Packages, [], State).


get_tag([], Accum, _State) ->
    {ok, lists:reverse(Accum)};
get_tag([#package{status=error}=Package|O], Accum, State) ->
    get_tag(O, [Package|Accum], State);
get_tag([Package|O], Accum, #state{vcs_plugin=VcsP, vcs_state=VcsS}=State) ->
    RR = State#state.repository_root,
    Name = Package#package.name,
    Branch = Package#package.branch,
    Revno = Package#package.current_revno,
    case catch VcsP:get_tag(VcsS, filename:join(RR, Name), Branch, Revno) of
        {ok, T} -> 
            get_tag(O, [Package#package{tag=T}|Accum], State);
        Error ->
            error_logger:error_msg("failed to get tag at ~p/~p with ~p~n", [Name, Package, Error]),
            get_tag(O, [Package|Accum], State)
    end.



build_result(Packages, _State) ->
    Notify = #notify{
        subject = <<>>,
        body = <<>>
    },
    case catch build_result(Packages, Notify, []) of
        {ok, {NewNotify, Archives}} ->
            NewPackages = [Package#package{diff= <<>>, changelog= <<>>} || Package <- Packages],
            {ok, #scan_pipe_result{notify=NewNotify, archives=Archives, packages=NewPackages}};
        Err ->
            error_logger:error_msg("build_result error: ~p~n", [Err]),
            {error, build_result}
    end.


build_result([], Notify, Archives) ->
    {ok, {Notify, lists:reverse(Archives)}};

build_result([Package|O], Notify, ArchiveAccum) ->
    Name = Package#package.name,
    Branch = Package#package.branch,
    {PackageBody, NewArchiveAccum} = case Package#package.status of
        ok ->
            Archive = #archive{
                name=Name, branch=Branch,
                archive_name=Package#package.archive_name,
                tag=Package#package.tag
            },
            {DiffLength, Diff} = limit_output(Package#package.diff, 10240),
            {_, ChangeLog} = limit_output(Package#package.changelog, 10240),
            Body = list_to_binary(
                io_lib:format(
                    "~s/~s~n~s~nDiff contains ~p bytes~n~s~n",
                    [Name, Branch, ChangeLog, DiffLength, Diff]
                )
            ),
            {Body,  [Archive|ArchiveAccum]};
        error ->
            FailedAt = Package#package.failed_at,
            Reason = Package#package.reason,
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
    build_result(O, Notify#notify{body=NewBody}, NewArchiveAccum).


-spec limit_output(binary(), non_neg_integer()) -> {non_neg_integer(), binary()}.
limit_output(Bin, Size) when is_binary(Bin), is_integer(Size), Size > 0 ->
    case size(Bin) of
        S when S =< Size -> {S, Bin};
        S -> {S, << (binary_part(Bin, {0, Size}))/binary, "..." >>}
    end.

