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
    filelib:ensure_dir(DetsFile),
    {ok, Dets} = dets:open_file(DetsFile, [{access, read_write}]),
    State = #state{
        ets = init_ets(proplists:get_value(idents, Args, [])),
        dets = Dets,
        deploy_script = proplists:get_value(deploy_script, Args),
        rotate = proplists:get_value(pkg_rotation, Args, 1)
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
    {noreply, State};
handle_info(_Message, State) ->
    {noreply, State}.


handle_cast({rotate, Deploy}, State) ->
    rotate(Deploy, State),
    {noreply, State};
handle_cast(_Message, State) ->
    {noreply, State}.


handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call({deploy, #deploy{}=D}, _, State) ->
    {reply, deploy(D, State), State};

handle_call(_Message, _From, State) ->
    {reply, {error, bad_msg}, State}.




terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%----------


register_as_service(Delay) ->
    erlang:send_after(Delay, self(), register_as_service).


init_ets(Idents) when is_list(Idents) ->
    %FIXME: check default values
    Ets = ets:new(?MODULE, [protected, named_table]),
    InsertFun = fun({Ident, Branches}) ->
        [
            begin
                caterpillar_utils:ensure_dir(Path),
                ets:insert(Ets, {{Ident, Branch}, Path}),
                ok
            end || {Branch, Path} <- Branches
        ]
    end,
    lists:foreach(InsertFun, Idents),
    Ets.



%---------- deploy pipe ----------%


deploy(Deploy, State) ->
    FunList = [
        {run_pre_deploy, fun run_pre_deploy/2},
        {find_deploy_paths, fun find_deploy_paths/2},
        {copy_packages, fun copy_packages/2},
        {run_post_deploy, fun run_post_deploy/2},
        {cast_rotate, fun cast_rotate/2}
    ],
    case caterpillar_utils:pipe(FunList, Deploy, State) of
        {ok, _} -> ok;
        Error -> Error
    end.


run_pre_deploy(Deploy, _State) -> 
    {ok, Deploy}.


find_deploy_paths(#deploy{ident=Ident}=Deploy, #state{ets=Ets}) -> 
    Select = [{
        {{'$1', '$2'}, '$3'}, 
        [{'orelse', {'==', '$1', Ident}, {'==', '$1', 'default'}}],
        [['$2', '$3']]
    }],
    SelectResult = [{Branch, Path} || [Branch, Path] <- ets:select(Ets, Select)],
    case SelectResult of
        [] ->
            %FIXME: notify about
            {error, no_deploy};
        _ -> 
            {SelectResult, Deploy}
    end.


copy_packages({Paths, #deploy{packages=Packages, ident=Ident}=Deploy}, #state{dets=Dets}=State) ->
    DefaultPathValue = caterpillar_utils:get_value_or_die(default, Paths),
    Fun = fun(#deploy_package{package=Package, name=Name, branch=Branch, fd=FD}) ->
        Path = proplists:get_value(Branch, Paths, DefaultPathValue),
        {ok, _} = file:copy(FD, filename:join(Path, Package)),
        dets:insert(Dets, {{Ident, Name, Branch}, Path, unixtime()})
    end,
    lists:foreach(Fun, Packages),
    {ok, Deploy}.



run_post_deploy(#deploy{post_deploy_actions=Actions}=Deploy, State) ->
    lists:foreach(
        fun({M, F, A}) -> apply(M, F, A) end,
        Actions
    ),
    {ok, Deploy}.


cast_rotate(Deploy, _State) ->
    gen_server:cast(self(), {rotate, Deploy}),
    {ok, Deploy}.



%---------- end of deploy pipe ----------%


rotate(#deploy{packages=Packages, ident=Ident}, #state{dets=Dets, rotate=Rotate}) ->
    Fun = fun(#deploy_package{name=Name, branch=Branch}) ->
        Select = [{
            {{'$1', '$2', '$3'}, '$4', '$5'},
            [{'==', '$1', Ident}, {'==', '$2', Name}, {'==', '$3', Branch}],
            [['$4', '$5']]
        }],
        SelectResult = lists:sort(
            fun([Package1, Time1], [Package2, Time2]) ->
                Time1 > Time2
            end, 
            dets:select(Dets, Select)
        ),
        case length(SelectResult) > Rotate of
            true -> delete(SelectResult, SelectResult - Rotate, Dets);
            false -> ok
        end
    end,
    lists:foreach(Fun, Packages).


delete(_, 0, _) -> ok;
delete([ [Package, _Time]|O ], Num, Dets) ->
    error_logger:info_msg("deleting ~p~n", [Package]),
    file:delete(Package),
    dets:match_delete(Dets, {'_', Package, '_'}),
    delete(O, Num-1, Dets).



unixtime() ->
    list_to_integer(lists:flatten(io_lib:format("~4..0B~6..0B~6..0B", tuple_to_list(now())))).
