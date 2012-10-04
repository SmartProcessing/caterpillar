-module(caterpillar_repository).

-behaviour(gen_server).

-include_lib("caterpillar_repository_internal.hrl").
-define(GV, proplists:get_value).


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
    State = vcs_init(
        #state{
            ets = ets:new(?MODULE, [named_table, protected]),
            dets = Dets, %{{Package, Branch}, ArchiveName, LastRevision, BuildId}
            repository_root = caterpillar_utils:ensure_dir(?GV(repository_root, Args, ?REPOSITORY_ROOT)),
            export_root = caterpillar_utils:ensure_dir(?GV(export_root, Args, ?EXPORT_ROOT)),
            archive_root = caterpillar_utils:ensure_dir(?GV(archive_root, Args, ?ARCHIVE_ROOT)),
            scan_interval = ?GV(scan_interval, Args, ?SCAN_INTERVAL) * 1000,
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
            {ok, Packages} ->
                gen_server:call(?MODULE, {new_packages, Packages}, infinity);
            Error ->
                error_logger:error_msg("scan pipe failed with: ~p~n", [Error])
        end
    end),
    {noreply, scan_repository(State)};
handle_info(_Msg, State) ->
    {noreply, State}.


handle_cast({clean_packages, Packages}, State) ->
    clean_packages(State, Packages),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.



handle_call(get_packages, _From, State) ->
    {reply, [], State};

handle_call({new_packages, _Packages}, _From, State) ->
    {reply, ok, State};

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
clean_packages(#state{dets=D, export_root=ER, archive_root=AR, repository_root=RR}=State, [Package|O]) ->
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



%----

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
            Accum
    end,
    find_modified_packages(O, NewAccum, State).


export_packages(Packages, State) -> 
    export_packages(Packages, [], State).


export_packages([], [], _State) ->
    {error, {export_packages, "nothing exported"}};

export_packages([], Accum, _State) ->
    {ok, lists:reverse(Accum)};

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
            Accum
    end,
    export_packages(O, NewAccum, State).


archive_packages(Packages, State) -> 
    archive_packages(Packages, [], State).


archive_packages([], [], _State) ->
    {error, {archive_packages, "nothing archived"}};

archive_packages([], Accum, _State) ->
    {ok, lists:reverse(Accum)};

archive_packages([Package|O], Accum, #state{export_root=ER, archive_root=AR}=State) ->
    PackageName = Package#package.name,
    PackageBranch = Package#package.branch,
    ExportPath = filename:join([ER, PackageName, PackageBranch]),
    Archive = filename:join(
        AR,
        caterpillar_utils:package_to_archive(PackageName, PackageBranch)
    ),
    Result = (catch begin
        Tar = case erl_tar:open(Archive, [write, compressed]) of
            {ok, T} -> T;
            ErrT -> throw(ErrT)
        end,
        case erl_tar:add(Tar, ExportPath, filename:join(PackageName, PackageBranch), []) of
            ok -> ok;
            ErrAd -> throw(ErrAd)
        end,
        erl_tar:close(Tar),
        ok
    end),
    NewAccum = case Result of
        ok -> [Package|Accum];
        Error ->
            error_logger:error_msg(
                "archive_packages error: ~p~n at ~p/~p~n",
                [Error, PackageName, PackageBranch]
            ),
            Accum
    end,
    archive_packages(O, NewAccum, State).


get_diff(Packages, State) ->
    get_diff(Packages, [], State).


get_diff([], Accum, _State) ->
    {ok, lists:reverse(Accum)};
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

