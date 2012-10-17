-module(caterpillar_simple_builder).

-include_lib("caterpillar_simple_builder_internal.hrl").
-include_lib("caterpillar.hrl").

-define(GVOD, caterpillar_utils:get_value_or_die).

-export([init_worker/1, changes/3]).



        


init_worker(Args) ->
    WorkIdFile = ?GVOD(work_id_file, Args),
    [ArchiveRoot, RepositoryRoot, DeployRoot] = [
        caterpillar_utils:ensure_dir(?GVOD(Root, Args)) ||
        Root <- [archive_root, repository_root, deploy_root]
    ],
    State = #state{
        work_id = caterpillar_utils:read_work_id(WorkIdFile),
        work_id_file = WorkIdFile,
        archive_root = ArchiveRoot,
        repository_root = RepositoryRoot,
        deploy_root=DeployRoot
    },
    {ok, State}.



changes(#state{work_id=WorkId}, WorkId, _) -> ok;
changes(State, WorkId, Archives) ->
    FunList = [
        {retrieve_archives, fun retrieve_archives/2},
        {unarchive, fun unarchive/2},
        {make_packages, fun make_packages/2},
        {pre_deploy, fun pre_deploy/2},
        {deploy, fun deploy/2},
        {post_deploy, fun post_deploy/2}
    ],
    caterpillar_utils:pipe(FunList, Archives, State#state{next_work_id=WorkId}).



deploy(_, _, _) -> ok.



retrieve_archives(Archives, State) ->
    retrieve_archives(Archives, [], State).


retrieve_archives([], Accum, _State) ->
    {ok, Accum};
retrieve_archives([#archive{archive_name=AN}=A|O], Accum, #state{archive_root=AR}=State) ->
    Name = filename:join(AR, AN),
    {ok, FD} = file:open(Name, [write]),
    RequestArchive = A#archive{fd=FD},
    case catch caterpillar_worker:retrieve_archive(RequestArchive) of
        ok ->
            file:close(FD),
            retrieve_archives(O, [A|Accum], State);
        Error ->
            error_logger:info_msg("caterpillar_simple_builder: request_packages error: ~p~n", [Error]),
            {error, {request_packages, Error}}
    end.


unarchive(Archives, State) ->
    unarchive(Archives, [], State).


unarchive([], Accum, _State) ->
    {ok, Accum};
unarchive([ #archive{name=Name, branch=Branch, archive_name=AR}|T ], Accum, State) ->
    ArchiveRoot = State#state.archive_root,
    RepositoryRoot = State#state.repository_root,
    ArchivePath = filename:join([ArchiveRoot, AR]),
    UnArchivePath = filename:join([RepositoryRoot, Name, Branch]),
    caterpillar_utils:del_dir(UnArchivePath),
    caterpillar_utils:ensure_dir(UnArchivePath),
    case erl_tar:extract(ArchivePath, [{cwd, UnArchivePath}, compressed]) of
        {error, Reason} ->
            error_logger:error_msg(
                "Failed to unarchive ~p with reason ~p~n",
                [Name, Reason]
            ),
            file:delete(ArchivePath),
            error;
        _ ->
            file:delete(ArchivePath),
            unarchive(T, State, [ Name|Accum ])
    end.


make_packages(Archives, State) ->
    error_logger:info_msg("making packages ~n"),
    Packages = [
        #package{name=Name, branch=Branch} || 
        #archive{name=Name, branch=Branch} <- Archives
    ],
    make_packages(Packages, [], State).


make_packages([], Accum, State) ->
    {ok, Accum};
make_packages([ #package{name=Name, branch=Branch}=Package|T ], Accum, State) ->
    UnArchivePath = filename:join([State#state.repository_root, Package, Branch]),
    DistDir = filename:join(UnArchivePath, "dist"),
    caterpillar_utils:del_dir(DistDir),
    NewPackage = case filelib:is_dir(UnArchivePath) of
        true ->
            Commands = lists:map(
                fun(Command) -> lists:flatten(io_lib:format(Command, [UnArchivePath])) end,
                [
                    "make -C ~s clean &>/dev/null | exit 0 ", %exit status always 0
                    "make -C ~s test DIST_DIR=dist",
                    "make -C ~s package DIST_DIR=dist"
                ]
            ),
            case catch make(Package, DistDir, Commands, State) of
                {ok, PackageResult} ->
                    Package#package{build_status=ok, package=PackageResult};
                {error, Log} ->
                    Package#package{build_status=error, log=Log};
                Error ->
                    error_logger:error_msg("make unknown error: ~p~n", [Error]),
                    {error, make_packages}
            end;
        _ ->
            error_logger:error_msg("not dir ~p~n", [UnArchivePath]),
            Package#package{build_status=error, log= <<"unarchive path not directory">>}
    end,
    make_packages(T, [ NewPackage|Accum ], State).


make(Package, DistDir, Commands, #state{deploy_root=DeployRoot}) ->
    case make(Package, Commands) of
        {ok, done} ->
            case filelib:wildcard(filename:join(DistDir, "*.deb")) of
                [Deb] ->
                    DebName = lists:last(filename:split(Deb)),
                    DeployName = filename:join(DeployRoot, DebName),
                    {ok, _} = file:copy(Deb, DeployName),
                    {ok, Deb};
                Other ->
                    error_logger:error_msg("cant find deb package, ~p~n", [Other]),
                    {error, <<"no package build">>}
            end;
        Error -> Error
    end.


make(_, []) ->
    {ok, done};
make(#package{name=Name, branch=Branch}=Package, [ Cmd|T ]) ->
    P = open_port({spawn, Cmd}, [binary, use_stdio, stderr_to_stdout, exit_status]),
    case receive_data_from_port() of
        {ok, 0, Log} ->
            error_logger:info_msg("~p succeed at ~s/~s~n", [Cmd, Name, Branch]),
            make(Package, T); 
        {ok, _, Log} ->
            error_logger:info_msg("~p failed at ~s/~s~n", [Cmd, Name, Branch]),
            {error, Log};
        {error, build_timeout} ->
            error_logger:error_msg(
                "build timeout, closing port:~p~n",
                [(catch erlang:port_close(P))]
            ),
            {error, <<"build_timeout">>};
        {error, Reason} ->
            {error, Reason}
    end.


receive_data_from_port() ->
    receive_data_from_port(<<>>).

receive_data_from_port(Log) ->
    receive
        {_Port, {data, Data}} ->
            receive_data_from_port(<<Log/binary, Data/binary>>);
        {_Port, {exit_status, Status}} ->
            {ok, Status, Log}
    after 300000 ->
        {error, build_timeout}
    end.



pre_deploy(Packages, #state{deploy_root=DR, next_work_id=NWI}) ->
    RawNotify = #notify{
        subject = list_to_binary(io_lib:format(<<"deploy for build ~p">>, [NWI])),
        body = <<>>
    },
    DeployFold = fun
        (#package{name=N, branch=B, build_status=ok, package=Package}, {Deploy, Notify}) ->
            {ok, Fd} = file:open(filename:join(DR, Package)),
            NewDeploy = Deploy#deploy{packages=[{Package, Fd}|Deploy#deploy.packages]},
            Body = list_to_binary(io_lib:format("ok ~s/~s~n", [N, B])),
            OldBody = Notify#notify.body,
            NewNotify = Notify#notify{body = <<OldBody/binary, Body/binary>>},
            {NewDeploy, NewNotify};
        (#package{name=N, branch=B, log = Log}, {Deploy, Notify}) ->
            Body = list_to_binary(io_lib:format("error ~s/~s~n", [N, B])),
            OldBody = Notify#notify.body,
            NewNotify = Notify#notify{body = <<OldBody/binary, Body/binary, Log/binary, $\n>>},
            {Deploy, NewNotify}
    end,
    {RawDeploy, NewNotify} = lists:foldl(DeployFold, {#deploy{packages=[]}, RawNotify}, Packages),
    {ok, RawDeploy#deploy{post_deploy_actions = [{caterpillar_event, sync_event, [{notify, NewNotify}]}]}}.


deploy(Deploy, State) ->
    NewState = case catch caterpillar_event:sync_event({deploy, Deploy}) of
        ok ->
            State#state{work_id = State#state.next_work_id};
        Error ->
            error_logger:error_msg("deploy failed with ~p~n", [Error]),
            State
    end,
    {ok, Deploy, NewState}.



post_deploy(#deploy{packages=Packages}, State) ->
    lists:map(fun({_, Fd}) -> file:close(Fd) end, Packages),
    {ok, State}.

