-module(caterpillar_repository).

-behaviour(gen_server).

-include_lib("caterpillar_repository_internal.hrl").

-export([start_link/1, stop/0]).
-export([init/1, handle_info/2, handle_cast/2, handle_call/3, terminate/2, code_change/3]).



start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).


stop() -> 
    gen_server:call(?MODULE, stop, infinity).


init(Args) ->
    Dets = case dets:open_file(proplists:get_value(repository_db, Args, ?DETS), [{access, read_write}]) of
        {ok, D} -> D;
        Error -> 
            error_logger:error_msg("caterpillar_repository dets initialization error: ~p~n", [Error]),
            throw({caterpillar_repository, {dets, Error}})
    end,
    State = vcs_init(
        #state{
            ets = ets:new(?MODULE, [named_table, protected]),
            dets = Dets, %{{Package, Branch}, ArchiveName, LastRevision, BuildId}
            repository_root = ensure_dir(proplists:get_value(repository_root, Args, ?REPOSITORY_ROOT)),
            export_root = ensure_dir(proplists:get_value(export_root, Args, ?EXPORT_ROOT)),
            archive_root = ensure_dir(proplists:get_value(archive_root, Args, ?ARCHIVE_ROOT)),
            scan_interval = proplists:get_value(scan_interval, Args, ?SCAN_INTERVAL) * 1000,
            scan_timer = scan_repository(0)
        },
        Args
    ),
    {ok, State}.


ensure_dir(Path) ->
    AbsPath = filename:absname(Path ++ "/"),
    filelib:ensure_dir(AbsPath),
    AbsPath.


handle_info(scan_repository, State) ->
    spawn(fun() ->
        case catch scan_pipe(State) of
            {ok, NewPackages} ->
                gen_server:call(?MODULE, {new_packages, NewPackages}, infinity);
            Error ->
                error_logger:error_msg("scan pipe failed with: ~p~n", [Error])
        end
    end),
    {noreply, scan_repository(State)};
handle_info(_Msg, State) ->
    {noreply, State}.


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



terminate(Reason, _State) ->
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
    VcsPlugin = proplists:get_value(vcs_plugin, Args),
    case VcsPlugin:init(proplists:get_value(vcs_plugin_init, Args, [])) of
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



%----

scan_pipe(State) ->
    FunList = [
        {get_packages, fun get_packages/2},
        {get_brances, fun get_branches/2},
        {clean_packages, fun clean_packages/2},
        {find_modified_packages, fun find_modified_packages/2},
        {export_packages, fun export_packages/2},
        {archive_packages, fun archive_packages/2}
    ],
    caterpillar_utils:pipe(FunList, none, State).



get_packages(_, #state{repository_root=RR, vcs_plugin=VCSPlugin, vcs_state=VCSState}) ->
    case caterpillar_utils:list_packages(RR) of
        {ok, []} -> {error, {get_packages, "nothing in repository"}};
        {ok, Packages} ->
            FilterFun = fun(Package) ->
                VCSPlugin:is_repository(VCSState, filename:join(RR, Package))
            end,
            case catch lists:filter(FilterFun, Packages) of
                [] -> {error, {get_packages, "no repositories available"}};
                Repos when is_list(Repos) -> {ok, Repos};
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
    AbsPackage = filename:join(RR, Package),
    NewAccum = case caterpillar_utils:list_packages(AbsPackage) of
        {ok, []} -> 
            error_logger:info_msg("no branches in ~p~n", [Package]),
            Accum;
        {ok, RawBranches} -> 
            FoldFun = fun(Branch, Acc) ->
                case catch VCSPlugin:is_branch(VCSState, AbsPackage, Branch) of
                    true ->
                        [{Package, Branch}|Acc];
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


clean_packages(Branches, #state{dets=Dets}) -> 
    DetsBranches = dets:select(Dets, [{{'$1', '_', '_', '_'}, [], ['$1']}]),
    case DetsBranches -- Branches of
        [] -> ok;
        ToClean -> gen_server:cast(?MODULE, {clean_packages, ToClean})
    end,
    {ok, Branches}.


find_modified_packages(Branches, State) ->
    find_modified_packages(Branches, [], State).


find_modified_packages([], [], _State) ->
    {error, {find_modified_packages, "no packages modified"}};

find_modified_packages([], Acc, _State) ->
    {ok, lists:reverse(Acc)};

find_modified_packages([{Package, Branch}|O], Accum, State) ->
    VCSPlugin = State#state.vcs_plugin,
    VCSState = State#state.vcs_state,
    RR = State#state.repository_root,
    AbsPackage = filename:join(RR, Package),
    DetsResult = dets:select(
        State#state.dets,
        [{{{'$1', '$2'}, '_', '$3', '_'}, [{'==', '$1', Package}, {'==', '$2', Branch}], ['$3']}]
    ),
    DetsRevno = case DetsResult of
        [] -> none;
        [R] -> R
    end,
    NewAccum = case catch VCSPlugin:get_revno(VCSState, AbsPackage, Branch) of
        {ok, Revno} when Revno /= DetsRevno -> [{Package, Branch, Revno}|Accum];
        {ok, _} -> Accum;
        Error -> 
            error_logger:error_msg(
                "find_modified_packages error: ~p~n on ~p/~p~n",
                [Error, Package, Branch]
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

export_packages([{Package, Branch, Rev}|O], Accum, #state{export_root=ER, repository_root=RR}=State) ->
    VCSPlugin = State#state.vcs_plugin,
    VCSState = State#state.vcs_state,
    AbsExport = filename:join([ER, Package, Branch]),
    AbsPackage = filename:join(RR, Package),
    caterpillar_utils:del_dir(AbsExport),
    filelib:ensure_dir(AbsExport ++ "/"),
    NewAccum = case catch VCSPlugin:export(VCSState, AbsPackage, Branch, AbsExport) of
        ok -> [{Package, Branch, Rev}|Accum];
        Error ->
            error_logger:error_msg(
                "export_packages error ~p~n at ~p/~p(~p)~n", [Error, Package, Branch, Rev]
            ),
            Accum
    end,
    export_packages(O, NewAccum, State).


archive_packages(_Files, _State) -> ok.
