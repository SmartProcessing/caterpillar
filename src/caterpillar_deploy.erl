-module(caterpillar_deploy).

-behaviour(gen_server).

-include_lib("kernel/include/file.hrl").
-include_lib("caterpillar_deploy_internal.hrl").
-include_lib("caterpillar.hrl").

-export([start_link/1, stop/0]).
-export([init/1, handle_info/2, handle_cast/2, handle_call/3, terminate/2, code_change/3]).



start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).


stop() ->
    ok = gen_server:call(?MODULE, stop).



init(Args) ->
    DetsFile = proplists:get_value(deploy_db, Args, ?DEPLOY_DATABASE),
    DeployScriptDelay = proplists:get_value(deploy_script_delay, Args, 10),
    DeployScriptMaxWaiting = proplists:get_value(deploy_script_max_waiting, Args, 2),
    DeployPath = filename:absname(proplists:get_value(deploy_path, Args, ?DEFAULT_DEPLOY_PATH)),
    filelib:ensure_dir(DetsFile),
    {ok, Dets} = dets:open_file(DetsFile, [{access, read_write}]),
    State = #state{
        ets = init_ets(proplists:get_value(idents, Args, []), DeployPath),
        dets = Dets,
        deploy_script = proplists:get_value(deploy_script, Args),
        rotate = proplists:get_value(rotate, Args, 1),
        deploy_script_timer = none,
        deploy_script_delay = DeployScriptDelay,
        deploy_info = [],
        deploy_max_waiting = DeployScriptMaxWaiting
    },
    register_as_service(0),
    {ok, State}.


handle_info({'DOWN', _, _, _, _}, State) ->
    register_as_service(0),
    {noreply, State#state{registered=false}};
handle_info(register_as_service, #state{registered=false}=State) ->
    NewState = case catch caterpillar_event:register_service(deploy) of
        {ok, Pid} ->
            erlang:monitor(process, Pid),
            State#state{registered=true};
        _ ->
            register_as_service(5000),
            State
    end,
    {noreply, NewState};
handle_info(run_deploy, State) ->
    error_logger:info_msg("running deploy~n"),
    run_deploy(State),
    {noreply, State};
handle_info(_Message, State) ->
    error_logger:error_msg("unknown message: ~p~n", [_Message]),
    {noreply, State}.


handle_cast(_Message, State) ->
    {noreply, State}.


handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call({deploy, #deploy{}=Deploy}, _, State) ->
    NewState = init_deploy(Deploy, State),
    copy_deploy(Deploy, State),
    {reply, ok, NewState};

handle_call(_Message, _From, State) ->
    {reply, {error, bad_msg}, State}.




terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%----------


register_as_service(Delay) ->
    erlang:send_after(Delay, self(), register_as_service).


init_ets(Idents) ->
    init_ets(Idents, ?DEFAULT_DEPLOY_PATH).


init_ets(Idents, DeployPath) ->
    ToAbsPath = fun(Path) ->
        NewPath = case Path of
            "/" ++ _ -> Path;
            _ -> filename:join(DeployPath, Path)
        end,
        caterpillar_utils:ensure_dir(NewPath),
        NewPath
    end,
    Ets = ets:new(?MODULE, [protected, named_table]),
    [
        ets:insert(Ets, {{Type, Branch, Arch}, ToAbsPath(Path)}) || 
        {Type, Archs} <- Idents, {Arch, Branches} <- Archs, {Branch, Path} <- Branches
    ],
    Ets.


%-----


%FIXME: save deploy info (maybe another dets tab?)
init_deploy(#deploy{}=Deploy, State) ->
    #state{
        deploy_script_delay = Delay, deploy_script_timer=Timer, 
        deploy_info=Info, deploy_max_waiting=MaxWaiting
    }=State,
    NewInfo = [Deploy|Info],
    NewTimer = case MaxWaiting =< length(NewInfo) of
        true when Timer /= none -> Timer;
        true ->
            erlang:send_after(Delay, self(), run_deploy);
        false ->
            catch erlang:cancel_timer(Timer),
            erlang:send_after(Delay, self(), run_deploy)
    end,
    State#state{deploy_script_timer=NewTimer, deploy_info=NewInfo}.


%-----


-spec copy_packages(#deploy{}, #state{}) -> {ok, done}|{error, term()}.
copy_deploy(Deploy, State) ->
    FunList = [
        {run_pre_deploy, fun run_pre_deploy/2},
        {find_deploy_paths, fun find_deploy_paths/2},
        {copy_packages, fun copy_packages/2},
        {rotate_packages, fun rotate_packages/2}
    ],
    case caterpillar_utils:pipe(FunList, Deploy, State) of
        {ok, _} -> {ok, done};
        Error -> Error
    end.


%-----


-spec run_deploy(#state{}) -> {ok, done}.
run_deploy(State) ->
    FunList = [
        {run_deploy_script, fun run_deploy_script/2},
        {run_post_deploy, fun run_post_deploy/2}
    ],
    caterpillar_utils:pipe(FunList, none, State),
    {ok, done}.


%-----


run_pre_deploy(Deploy, _State) -> 
    %FIXME:
    {ok, Deploy}.


find_deploy_paths(#deploy{ident=#ident{arch=Arch, type=Type}}=Deploy, #state{ets=Ets}) -> 
    Select = [
        {
            {{'$1', '$2', '$3'}, '$4'}, 
            [
                {'orelse', {'==', '$1', Type}, {'==', '$1', default}},
                {'orelse', {'==', '$3', Arch}, {'==', '$3', default}}
            ],
            [['$1', '$2', '$3', '$4']]
        }
    ],
    SelectResult = [{{Type, Branch, Arch}, Path} || [Type, Branch, Arch, Path] <- ets:select(Ets, Select)],
    case SelectResult of
        [] ->
            %FIXME: notify about
            {error, no_deploy};
        _ -> 
            {ok, {SelectResult, Deploy}}
    end.


copy_packages({Paths, #deploy{packages=Packages, ident=#ident{arch=Arch, type=Type}}=Deploy}, #state{dets=Dets}) ->
    DefaultPathValue = case proplists:get_value({Type, default, Arch}, Paths, '$none$') of
        '$none$' ->
            case proplists:get_value({default, default, default}, Paths, '$none') of
                '$none' ->
                    error_logger:error_msg("no default path exists~n"),
                    exit({copy_packages, no_default_path});
                Val -> Val
            end;
        DefaultPath -> DefaultPath
    end,
    Fun = fun(#deploy_package{package=Package, name=Name, branch=Branch, fd=FD}) ->
        Path = proplists:get_value({Type, Branch, Arch}, Paths, DefaultPathValue),
        AbsPackage = filename:join(Path, Package),
        error_logger:info_msg("deploying to ~p~n", [AbsPackage]),
        {ok, _} = file:copy(FD, AbsPackage),
        dets:insert(Dets, {{Type, Arch, AbsPackage}, {Name, Branch}, unixtime()})
    end,
    lists:foreach(Fun, Packages),
    {ok, Deploy}.



run_post_deploy(#deploy{post_deploy_actions=Actions}=Deploy, _State) when is_list(Actions) ->
    lists:foreach(
        fun
            ({M, F, A}) -> error_logger:info_msg("post_deploy ~p/~p result: ~p~n", [M, F, catch apply(M, F, A)]);
            (Bad) -> error_logger:error_msg( "post_deploy badarg: ~p~n", [Bad])
        end,
        Actions
    ),
    {ok, Deploy};
run_post_deploy(#deploy{post_deploy_actions=BadAction}=Deploy, _State) ->
    error_logger:error_msg("bad post_deploy actions: ~p~n", [BadAction]),
    {ok, Deploy}.


rotate_packages(#deploy{packages=Packages, ident=#ident{arch=Arch, type=Type}=Ident}, #state{dets=Dets, rotate=Rotate}) ->
    error_logger:info_msg("rotating packages~n"),
    Fun = fun(#deploy_package{name=Name, branch=Branch}) ->
        Select = [{
            {{'$1', '$2', '$3'}, {'$4', '$5'}, '$6'},
            [
                {'==', '$1', Type},
                {'==', '$2', Arch},
                {'==', '$4', Name},
                {'==', '$5', Branch}
            ],
            [['$3', '$6']]
        }],
        SelectResult = lists:sort(
            fun([_Package1, Time1], [_Package2, Time2]) -> Time1 =< Time2 end, 
            dets:select(Dets, Select)
        ),
        case length(SelectResult) > Rotate of
            true -> delete(SelectResult, length(SelectResult) - Rotate, Dets);
            false -> ok
        end
    end,
    lists:foreach(Fun, Packages),
    {ok, done}.


run_deploy_script(_, #state{deploy_script=Script, deploy_info=Info}) ->
    PackagesFoldFun = fun(#deploy_package{branch=Branch}, {Arch, Type, Accum}) ->
        Element = {Type, Branch, Arch},
        NewAccum = case lists:member(Element, Accum) of
            true -> Accum;
            false -> [Element|Accum]
        end,
        {Arch, Type, NewAccum}
    end,
    DeployFoldFun = fun(#deploy{packages=Packages, ident=#ident{arch=Arch, type=Type}}, Accum) ->
        {Arch, Type, NewAccum} = lists:foldl(PackagesFoldFun, {Arch, Type, Accum}, Packages),
        NewAccum
    end,
    ScriptInfo = lists:foldl(DeployFoldFun, [], Info),
    case ScriptInfo of
        [] -> 
            error_logger:error_msg("nothing to update, deploy_info: ~p~n", [Info]);
        _ ->
            Commands = [lists:flatten(io_lib:format("~s ~s ~s ~s", [Script, Type, Branch, Arch])) || {Type, Branch, Arch} <- ScriptInfo],
            UpdateFun = fun(Cmd) ->
                error_logger:info_msg("running:~s~n", [Cmd]),
                error_logger:info_msg("deploy script result: ~p~n", [os:cmd(Cmd)])
            end,
            lists:foreach(UpdateFun, Commands)
    end,
    {ok, done}.


%---------- end of deploy pipe ----------%




delete(_, 0, _) -> ok;
delete([ [Package, _Time]|O ], Num, Dets) ->
    error_logger:info_msg("deleting ~p~n", [Package]),
    file:delete(Package),
    dets:match_delete(Dets, {{'_', '_', Package}, '_', '_'}),
    delete(O, Num-1, Dets).



unixtime() ->
    list_to_integer(lists:flatten(io_lib:format("~4..0B~6..0B~6..0B", tuple_to_list(now())))).
