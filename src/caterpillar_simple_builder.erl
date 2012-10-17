-module(caterpillar_simple_builder).

-include_lib("caterpillar_simple_builder_internal.hrl").
-include_lib("caterpillar.hrl").

-define(GVOD, caterpillar_utils:get_value_or_die).

-export([init_worker/1, changes/3]).



        


init_worker(Args) ->
    WorkIdFile = ?GVOD(work_id_file, Args),
    ArchiveRoot = caterpillar_utils:ensure_dir(?GVOD(archive_root, Args)),
    RepositoryRoot = caterpillar_utils:ensure_dir(?GVOD(repository_root, Args)),
    State = #state{
        work_id = caterpillar_utils:read_work_id(WorkIdFile),
        work_id_file = WorkIdFile,
        archive_root = ArchiveRoot,
        repository_root = RepositoryRoot
    },
    {ok, State}.






changes(#state{work_id=WorkId}, WorkId, _) -> ok;
changes(State, WorkId, Archives) ->
    FunList = [
        {request_packages, fun request_packages/2},
        {unpack, fun unpack/2},
        {clean_dist, fun clean_dist/2},
        {make_packages, fun make_packages/2},
        %{copy_package, fun copy_package/2},
        {deploy, fun deploy/2}
    ],
    caterpillar_utils:pipe(FunList, Archives, State).



request_packages(Archives, State) ->
    request_packages(Archives, [], State).


request_packages([], Accum, _State) ->
    {ok, Accum};
request_packages([#archive{}=A|O], Accum, State) ->
    request_packages(O, [A|Accum], State).


unpack(Archives, State) ->
    unpack(Archives, State, []).


unpack([], _State, Accum) ->
    {ok, Accum};
unpack([ {Repo, Branch}=Name|T ], State, Accum) ->
    PackName = buildnet_utils:branch_to_archive(Repo, Branch),
    ArchiveRoot = State#state.archive_root,
    UnpackDir = State#state.repository_root,
    PackPath = filename:join([ArchiveRoot, PackName]),
    UnpackPath = filename:join([UnpackDir, Repo, Branch]),
    buildnet_utils:del_dir(UnpackPath),
    case buildnet_tar:extract(PackPath, [{cwd, UnpackDir}, compressed]) of
        {error, Reason} ->
            error_logger:error_msg(
                "Failed to unpack ~p with reason ~p~n",
                [Name, Reason]
            ),
            file:delete(PackPath),
            error;
        _ ->
            file:delete(PackPath),
            unpack(T, State, [ Name|Accum ])
    end.


clean_dist(Names, State) ->
    error_logger:info_msg("cleaning up dist dir~n"),
    {ok, Names}.


make_packages(Names, State) ->
    error_logger:info_msg("making packages ~n"),
    make_packages(Names, State, []).


make_packages([], _State, Accum) ->
    {ok, Accum};
make_packages([ {Package, Branch}|T ], State, States) ->
    UnpackPath = filename:join([State#state.repository_root, Package, Branch]),
    case filelib:is_dir(UnpackPath) of
        true ->
            Result = make(
                {Package, Branch, UnpackPath},
                [
                    "make clean &>/dev/null | exit 0 ", %exit status always 0
                    "make test DIST_DIR=dist",
                    "make package DIST_DIR=dist"
                ]
            ),
            NewResult = case Result of
                {ok, Package, Branch} ->
                    Pkg = copy_package(
                        Package, %FIXME
                        Package, filename:join(UnpackPath, "dist")
                    ),
                    {ok, Package, Branch, Pkg};
                Other ->
                    Other
            end,
            make_packages(T, State, [ NewResult|States ]);
        _ ->
            error_logger:error_msg(
                "not dir ~p~n~p~n",
                [UnpackPath, file:get_cwd()]
            ),
            make_packages(
                T,
                State,
                [ {error, Package, "bad package"}|States ]
            )
    end.


copy_package(DistDir, Package, SearchDir) ->
    file:set_cwd(SearchDir),
    case filelib:wildcard(Package ++ "*.deb") of
        [Pkg] ->
            DistPkg = filename:join(DistDir, Pkg),
            case file:copy(Pkg, DistPkg) of 
                {ok, _} ->
                    Pkg;
                Other ->
                    Other
            end;
        _ ->
            {error, "no package built"}
    end.


make({Package, Branch, _UnpackPath}, []) ->
    {ok, Package, Branch};
make({Package, Branch, UnpackPath}, [ Cmd|T ]) ->
    file:set_cwd(UnpackPath),
    P = open_port({spawn, Cmd}, [binary, use_stdio, stderr_to_stdout, exit_status]),
    case receive_data_from_port() of
        {ok, 0, Log} ->
            error_logger:info_msg("~p succeed at ~s/~s~n", [Cmd, Package, Branch]),
            make({Package, Branch, UnpackPath}, T); 
        {ok, _, Log} ->
            error_logger:info_msg("~p failed at ~s/~s~n", [Cmd, Package, Branch]),
            {error, Package, Branch, Log};
        {error, build_timeout} ->
            error_logger:error_msg(
                "build timeout, closing port:~p~n",
                [(catch erlang:port_close(P))]
            ),
            {error, Package, Branch, build_timeout};
        {error, Reason} ->
            {error, Package, Branch, Reason}
    end.


receive_data_from_port() ->
    receive_data_from_port(<<>>).

receive_data_from_port(Log) ->
    receive
        {_Port, {data, Data}} ->
            receive_data_from_port(<<Log/binary, Data/binary>>);
        {_Port, {exit_status, Status}} ->
            {ok, Status, Log}
    after 3000 ->
        {error, build_timeout}
    end.


deploy(ResultList, State) ->
    Bid = State#state.work_id,
    {S, E} = deploy(ResultList, State, [], []),
    error_logger:info_msg(
        "deploying packages ~p to master~n",
        [[ Package || {ok, Package, _} <- S ]]
    ),
    Result = lists:flatten([S, E]),
    Return = case catch gen_server:call(
            {global, test}, {deploy, Bid, ident, Result}, infinity) of
        ok ->
            {ok, Result};
        {'EXIT', Reason} ->
            error_logger:error_msg("deploy failed with reason ~p~n", [Reason]),
            error;
        Other ->
            error_logger:error_msg("deploy got unknown message ~p~n", [Other]),
            error
    end,
    %for sure
    Return.




deploy([], _State, SuccessList, ErrorList) ->
    {SuccessList, ErrorList};
deploy([ H|T ], State, SuccessList, ErrorList) ->
    Bid = integer_to_list(State#state.work_id),
    case H of
        {ok, Repo, Branch, {error, Reason}} ->
            deploy(
                T,
                State,
                SuccessList,
                [ {error, {Repo, Branch}, Reason}|ErrorList ]
            );
        {ok, Repo, Branch, Pkg} ->
            {ok, F} = file:open(filename:join(Repo, Pkg), [read]),
            deploy(
                T,
                State,
                [ {ok, {Repo, Branch}, {Pkg, F}}|SuccessList ],
                ErrorList
            );
        {error, Package, Branch, Log} ->
            deploy(T, State, SuccessList, [{error, {Package, Branch}, Log}|ErrorList]);
        Other ->
            error_logger:error_msg("deploy errorous message ~p~n", [Other]),
            error
    end.
    


