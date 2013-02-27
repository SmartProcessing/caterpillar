-module(caterpillar_simple_builder).

-include_lib("caterpillar_simple_builder_internal.hrl").
-include_lib("caterpillar.hrl").

-define(GVOD, caterpillar_utils:get_value_or_die).

-export([init_worker/2, changes/3, get_work_id/1, terminate_worker/1]).


init_worker(Ident, Args) when is_atom(Ident) ->
    WorkIdFile = ?GVOD(work_id_file, Args),
    [ArchiveRoot, RepositoryRoot, DeployRoot] = [
        caterpillar_utils:ensure_dir(?GVOD(Root, Args)) ||
        Root <- [archive_root, repository_root, deploy_root]
    ],
    State = #state{
        ident=Ident,
        work_id = caterpillar_utils:read_work_id(WorkIdFile),
        work_id_file = WorkIdFile,
        archive_root = ArchiveRoot,
        repository_root = RepositoryRoot,
        deploy_root=DeployRoot
    },
    {ok, State};
init_worker(BadIdent, _Args) ->
    error_logger:error_msg("bad ident: ~p~n", [BadIdent]),
    {error, bad_ident}.


terminate_worker(_State) -> ok.


changes(#state{work_id=WorkId}=State, WorkId, _) -> {ok, State};
changes(State, _, []) -> {ok, State};
changes(State, WorkId, Archives) ->
    FunList = [
        {retrieve_archives, fun retrieve_archives/2},
        {unarchive, fun unarchive/2},
        {clean_deploy_root, fun clean_deploy_root/2},
        {make_packages, fun make_packages/2},
        {pre_deploy, fun pre_deploy/2},
        {deploy, fun deploy/2},
        {post_deploy, fun post_deploy/2}
    ],
    caterpillar_utils:pipe(FunList, Archives, State#state{next_work_id=WorkId}).



get_work_id(#state{work_id=WI}) -> {ok, WI}.



%--------------------


retrieve_archives(Archives, State) ->
    retrieve_archives(Archives, [], State).


retrieve_archives([], Accum, _State) ->
    {ok, Accum};
retrieve_archives([#archive{archive_name=AN}=A|O], Accum, #state{archive_root=AR}=State) ->
    Name = caterpillar_utils:filename_join(AR, AN),
    {ok, FD} = file:open(Name, [write]),
    RequestArchive = A#archive{fd=FD},
    case catch caterpillar_worker:retrieve_archive(RequestArchive) of
        ok ->
            file:close(FD),
            error_logger:info_msg("~s/~s retrieved~n", [A#archive.name, A#archive.branch]),
            retrieve_archives(O, [A|Accum], State);
        Error ->
            error_logger:info_msg(
                "caterpillar_simple_builder: request_packages error on ~s/~s: ~p~n",
                [A#archive.name, A#archive.branch, Error]
            ),
            {error, {request_packages, Error}}
    end.


unarchive(Archives, State) ->
    unarchive(Archives, [], State).


unarchive([], Accum, _State) ->
    {ok, Accum};
unarchive([ #archive{name=Name, branch=Branch, archive_name=AR}=A|T ], Accum, State) ->
    ArchiveRoot = State#state.archive_root,
    RepositoryRoot = State#state.repository_root,
    ArchivePath = caterpillar_utils:filename_join([ArchiveRoot, AR]),
    UnArchivePath = caterpillar_utils:filename_join([RepositoryRoot, Name, Branch]),
    caterpillar_utils:del_dir(UnArchivePath),
    caterpillar_utils:ensure_dir(UnArchivePath),
    case caterpillar_tar:extract(ArchivePath, [{cwd, UnArchivePath}, compressed, verbose]) of
        {error, Reason} ->
            error_logger:error_msg(
                "Failed to unarchive ~p with reason ~p~n",
                [Name, Reason]
            ),
            file:delete(ArchivePath),
            error;
        _ ->
            file:delete(ArchivePath),
            error_logger:info_msg("~s/~s unarchived~n", [Name, Branch]),
            unarchive(T, [ A|Accum ], State)
    end.


clean_deploy_root(Archives, #state{deploy_root=DR}) ->
    caterpillar_utils:del_dir(DR),
    caterpillar_utils:ensure_dir(DR),
    {ok, Archives}.


make_packages(Archives, State) ->
    error_logger:info_msg("making packages ~n"),
    Packages = [
        #package{name=Name, branch=Branch, tag=Tag} || 
        #archive{name=Name, branch=Branch, tag=Tag} <- Archives
    ],
    make_packages(Packages, [], State).


make_packages([], Accum, _State) ->
    {ok, Accum};
make_packages([ #package{name=Name, branch=Branch}=Package|T ], Accum, #state{next_work_id=WorkId}=State) ->
    UnArchivePath = caterpillar_utils:filename_join([State#state.repository_root, Name, Branch]),
    ControlFile = caterpillar_utils:filename_join(UnArchivePath, "control"),
    case filelib:is_regular(ControlFile) of
        true -> catch modify_control(ControlFile, Branch, WorkId, State#state.ident);
        _ -> ok
    end,
    DistDir = caterpillar_utils:filename_join(UnArchivePath, "dist"),
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
            case filelib:wildcard(caterpillar_utils:filename_join(DistDir, "*.deb")) of
                [Deb] ->
                    DebName = lists:last(filename:split(Deb)),
                    DeployName = caterpillar_utils:filename_join(DeployRoot, DebName),
                    {ok, _} = file:copy(Deb, DeployName),
                    {ok, DebName};
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
        {ok, 0, _Log} ->
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



pre_deploy(Packages, #state{deploy_root=DR, next_work_id=NWI, ident=Ident}) ->
    RawNotify = #notify{
        subject = list_to_binary(io_lib:format(<<"deploy for build ~p">>, [NWI])),
        body = <<>>
    },
    DeployFold = fun
        (#package{name=N, branch=B, build_status=ok, package=Package}, {Deploy, Notify}) ->
            error_logger:info_msg("openning ~p~n", [Package]),
            {ok, Fd} = file:open(caterpillar_utils:filename_join(DR, Package), [read, binary]),
            NewDeploy = Deploy#deploy{
                packages=[
                    #deploy_package{name=N, branch=B, package=Package, fd=Fd}|
                    Deploy#deploy.packages
                ]
            },
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
    Deploy = RawDeploy#deploy{
        post_deploy_actions = [{caterpillar_event, sync_event, [{notify, NewNotify}]}],
        ident=Ident,
        work_id=NWI
    },
    {ok, Deploy}.



deploy(Deploy, State) ->
    NewState = case catch caterpillar_worker:deploy(Deploy) of
        ok ->
            caterpillar_utils:write_work_id(State#state.work_id_file, State#state.next_work_id),
            State#state{work_id = State#state.next_work_id};
        Error ->
            error_logger:error_msg("deploy failed with ~p~n", [Error]),
            State
    end,
    {ok, Deploy, NewState}.



post_deploy(#deploy{packages=Packages}, State) ->
    lists:map(fun(#deploy_package{fd=Fd}) -> file:close(Fd) end, Packages),
    {ok, State}.



%----------



modify_control(ControlFile, Branch, Revision, Ident) ->
    {ok, Data} = file:read_file(ControlFile),
    NewData = lists:map(
        fun(Bin) ->
            NewBin = case split(Bin, <<":">>, []) of
                [<<"Version">>, Version] ->
                    RawVersion = pick_out_version(string:strip(binary_to_list(Version), right, $ ), []),
                    NewVersion = list_to_binary(RawVersion++"-"++Branch++"."++integer_to_list(Revision)),
                    <<"Version:", NewVersion/binary>>;
                [<<"Section">>, Section] ->
                    RawSection = string:strip(binary_to_list(Section), right, $ ),
                    NewSection = list_to_binary(RawSection++"-"++Branch),
                    <<"Section:", NewSection/binary>>;
                [<<"Architecture">>, Arch] ->
                    NewArch = case string:strip(binary_to_list(Arch), both, $ ) of
                        "all" -> <<"all">>;
                        _ -> atom_to_binary(Ident, latin1)
                    end,
                    <<"Architecture: ", NewArch/binary>>;
                _ -> Bin

            end,
            <<NewBin/binary, "\n">>
        end,
        split(Data, <<"\n">>, [global])
    ),
    file:write_file(ControlFile, NewData).
    

split(<<>>, _, _) -> <<>>;
split(Bin, D, Opts) -> binary:split(Bin, D, Opts).


pick_out_version([], Buf) ->
    lists:reverse(Buf);
pick_out_version([$-|_T], Buf) ->
    pick_out_version([], Buf);
pick_out_version([H|T], Buf) ->
    pick_out_version(T, [H|Buf]).

