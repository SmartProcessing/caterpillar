-module(caterpillar_repository_tests).

-on_load(tty_off/0).

-include_lib("eunit/include/eunit.hrl").
-include_lib("caterpillar_repository_internal.hrl").

-define(ARGS, [{vcs_plugin, test_vcs_plugin}, {repository_db, "repo.db"}]).


tty_off() ->
    error_logger:tty(false).


tty_on() ->
    error_logger:tty(true).



start_link_test_() ->
{setup,
    fun() -> ok end,
    fun(_) -> ok = caterpillar_repository:stop(), file:delete("repo.db") end,
    fun() ->
        Res = caterpillar_repository:start_link(?ARGS),
        ?assertMatch({ok, _}, Res),
        {ok, Pid} = Res,
        ?assertEqual(Pid, whereis(caterpillar_repository))
    end
}.



stop_test_() ->
{setup,
    fun() -> {ok, Pid} = caterpillar_repository:start_link(?ARGS) end,
    fun(_) -> catch erlang:exit(whereis(caterpillar_repository), kill), file:delete("repo.db") end,
    fun() ->
        Pid = whereis(caterpillar_repository),
        ?assert(is_pid(Pid)),
        ?assert(is_process_alive(Pid)),
        ?assertEqual(ok, caterpillar_repository:stop()),
        timer:sleep(1),
        ?assert(not is_process_alive(Pid))
    end
}.


vcs_init_test_() ->
{foreach,
    fun() -> ok end,
[
    {Message, fun() ->
        ?assertEqual(
            Result,
            catch caterpillar_repository:vcs_init(#state{}, Args)
        )
    end} || {Message, Args, Result} <- [
        {
            "vcs inited",
            [{vcs_plugin, test_vcs_plugin}],
            #state{vcs_plugin=test_vcs_plugin, vcs_state=state}
        },
        {
            "bad vcs callback, init failed",
            [{vcs_plugin, undefined}],
            {'EXIT', {vcs_init, failed}}
        }
    ]
]}.


scan_repository_test_() ->
{foreach,
    fun() -> ok end,
[
    {Message, fun() ->
        Check(caterpillar_repository:scan_repository(Data()))        
    end} || {Message, Check, Data} <- [
        {
            "scan_repository received integer as parameter",
            fun(Ref) ->
                ?assert(is_reference(Ref)),
                ?assert(erlang:cancel_timer(Ref) > 0)
            end,
            fun() -> 1000 end
        },
        {
            "scan_repository received integer as parameter, timer fires",
            fun(_Ref) ->
                timer:sleep(1),
                ?assertEqual(
                    receive Any -> Any after 10 -> timeout end,
                    scan_repository
                )
            end,
            fun() -> 0 end
        },
        {
            "scan_repository received state as parameter with not cancelled timer, timer fires",
            fun(_Ref) ->
                ?assertEqual(
                    receive Any -> Any after 10 -> timeout end,
                    scan_repository
                )
            end,
            fun() -> #state{
                    scan_timer=erlang:send_after(10, self(), ':D'),
                    scan_interval=1
                }
            end
        }
    ]
]}.


init_test_() ->
{foreach,
    fun() -> ok end,
    fun(_) -> file:delete("repo.db") end,
[
    {"successful init", fun() ->
        Res = caterpillar_repository:init([{vcs_plugin, test_vcs_plugin}, {repository_db, "repo.db"}]),
        ?assertMatch({ok, _}, Res),
        {ok, State} = Res,
        ?assertEqual(State#state.scan_interval, ?SCAN_INTERVAL * 1000),
        ?assertEqual(erlang:read_timer(State#state.scan_timer), 0),
        ?assertEqual(State#state.archive_root, ?ARCHIVE_ROOT),
        ?assertEqual(State#state.repository_root, ?REPOSITORY_ROOT),
        ?assertEqual(State#state.vcs_plugin, test_vcs_plugin),
        ?assertEqual(State#state.vcs_state, state),
        ?assertEqual(ets:info(State#state.ets), ets:info(caterpillar_repository)),
        ?assertEqual(State#state.dets, "repo.db")
    end},
    {"init failed, cant reach dets database", fun() ->
        ?assertEqual(
            {caterpillar_repository, {dets, {error, {file_error, "no_such_directory/repo_db", enoent}}}},
            catch caterpillar_repository:init([{repository_db, "no_such_directory/repo_db"}])
        )
    end}
]}. 



get_packages_test_() ->
{foreachx,
    fun(Directories) ->
        [filelib:ensure_dir(Dir) || Dir <- Directories],
        #state{repository_root="__test", vcs_plugin=test_vcs_plugin}
    end,
    fun(Directories, _) -> [caterpillar_utils:del_dir(Dir) || Dir <- Directories ++ ["__test"]] end,
[
    {Setup, fun(_, State) ->
        {Message, fun() ->
            ?assertEqual(
                Result,
                caterpillar_repository:get_packages('_', State)
            )
        end}
    end} || {Message, Setup, Result} <- [
        {
            "listing packages in not empty repo",
            ["__test/package1/", "__test/package2/", "__test/package3/"],
            {ok, ["package1", "package2"]}
        },
        {
            "repository root not exists",
            [],
            {error, {get_packages, {error, enoent}}}
        },
        {
            "repository root empty",
            ["__test/"],
            {error, {get_packages, "nothing in repository"}}
        },
        {
            "repository plugin exits",
            ["__test/exit/"],
            {error, {get_packages, {plugin_bad_return, {'EXIT',some_reason}}}}
        },
        {
            "repository plugin throws exception",
            ["__test/throw/"],
            {error, {get_packages, {plugin_bad_return, some_reason}}}
        }
    ]
]}.


get_branches_test_() ->
{foreachx,
    fun(Directories) ->
        [filelib:ensure_dir(Dir) || Dir <- Directories],
        #state{repository_root="__test", vcs_plugin=test_vcs_plugin}
    end,
    fun(Directories, _) -> [caterpillar_utils:del_dir(Dir) || Dir <- Directories ++ ["__test"]] end,
[
    {Setup, fun(_, State) ->
        {Message, fun() ->
            ?assertEqual(
                Result,
                catch caterpillar_repository:get_branches(Packages, State)
            )
        end} 
    end} || {Message, Setup, Packages, Result} <- [
        {
            "no branches in repos",
            ["__test/package1/", "__test/package2/"],
            ["package1", "package2"],
            {error, {get_branches, "no branches in repositories"}}
        },
        {
            "one branch in one repo",
            ["__test/package1/branch1/", "__test/package2/"],
            ["package1"],
            {ok, [{"package1", "branch1"}]}
        },
        {
            "few branches in different repos",
            ["__test/package1/branch1/", "__test/package2/branch2/"],
            ["package1", "package2"],
            {ok, [{"package1", "branch1"}, {"package2", "branch2"}]}
        },
        {
            "plugin exits on branch check",
            [
                "__test/package1/exit/", "__test/package1/branch1/",
                "__test/package2/throw/", "__test/package2/branch2/"
            ],
            ["package1", "package2"],
            {ok, [{"package1", "branch1"}, {"package2", "branch2"}]}
        }
    ]
]}.


clean_packages_test_() ->
{foreachx,
    fun(Setup) ->
        {ok, D} = dets:open_file("test.dets", [{access, read_write}]),
        [dets:insert(D, {X, archive, revision, build_id}) || X <- Setup],
        #state{dets=D}
    end,
    fun(_, _) ->
        dets:close("test.dets"), file:delete("test.dets")
    end,
[
    {Setup, fun(_, State) ->
        {Message, fun() ->
            register(caterpillar_repository, self()),
            ?assertEqual(
                {ok, Branches},
                caterpillar_repository:clean_packages(Branches, State)
            ),
            ?assertEqual(
                RecvResult,
                receive Msg -> Msg after 10 -> timeout end
            )
        end}
    end} || {Message, Setup, Branches, RecvResult} <- [
        {
            "nothing in dets, clean message not sent",
            [],
            [{"package1", "branch1"}, {"package2", "branch2"}],
            timeout
        },
        {
            "something in dets, but nothing to delete",
            [{"package1", "branch1"}],
            [{"package1", "branch1"}, {"package2", "branch2"}],
            timeout
        },
        {
            "deleting missing branches n packages",
            [{"package1", "branch1"}, {"package3", "branch3"}],
            [{"package1", "branch1"}, {"package2", "branch2"}],
            {'$gen_cast', {clean_packages, [{"package3","branch3"}]}}
        }
    ]
]}.



find_modified_packages_test_() ->
{foreachx,
    fun(Setup) ->
        {ok, D} = dets:open_file("test.dets", [{access, read_write}]),
        [dets:insert(D, {X, archive, R, build_id}) || {X, R} <- Setup],
        #state{dets=D, vcs_plugin=test_vcs_plugin, repository_root="__test"}
    end,
    fun(_, _) ->
        dets:close("test.dets"), file:delete("test.dets")
    end,
[
    {Setup, fun(_, State) ->
        {Message, fun() ->
            ?assertEqual(
                Result,
                caterpillar_repository:find_modified_packages(Branches, State)
            )
        end}
    end} || {Message, Setup,  Branches, Result} <- [
        {
            "new package (package2/branch2)",
            [{{"package1", "branch1"}, 1}],
            [{"package1", "branch1"}, {"package2", "branch2"}],
            {ok, [{"package2", "branch2", 1}]}
        },
        {
            "both packages not modified",
            [{{"package1", "branch1"}, 1}, {{"package2", "branch2"}, 1}],
            [{"package1", "branch1"}, {"package2", "branch2"}],
            {error,{find_modified_packages,"no packages modified"}}
        },
        {
            "both packages modified",
            [{{"package1", "branch1"}, 10}, {{"package2", "branch2"}, 10}],
            [{"package1", "branch1"}, {"package2", "branch2"}],
            {ok, [{"package1", "branch1", 1}, {"package2", "branch2", 1}]}
        },
        {
            "plugin returns bad response",
            [],
            [{"crash", "me"}],
            {error, {find_modified_packages, "no packages modified"}}
        }
    ]
]}.


export_packages_test_() ->
{foreachx,
    fun(Directories) ->
        [filelib:ensure_dir(Dir) || Dir <- Directories],
        #state{repository_root="__test", export_root="__test_export", vcs_plugin=test_vcs_plugin}
    end,
    fun(_, #state{repository_root=RR, export_root=ER}) ->
        [caterpillar_utils:del_dir(D) || D <- [RR, ER]]
    end,
[
    {Setup, fun(_, State) ->
        {Message, fun() ->
            ?assertEqual(
                Result,
                caterpillar_repository:export_packages(Packages, State)
            ),
            RR = State#state.repository_root,
            ER = State#state.export_root,
            RepoPackages = caterpillar_utils:list_packages(RR),
            ExprPackages = caterpillar_utils:list_packages(RR),
            ?assertEqual(
                RepoPackages,
                ExprPackages
            ),
            {ok, ListedPackages} = RepoPackages,
            [?assertEqual(
                caterpillar_utils:list_packages(filename:join(RR, Package)),
                caterpillar_utils:list_packages(filename:join(ER, Package))
            ) || Package <- ListedPackages],
            Check()
        end}
    end} || {Message, Setup, Packages, Check, Result} <- [
        {
            "nothing exported",
            ["__test/"],
            [],
            fun() -> ok end,
            {error, {export_packages, "nothing exported"}}
        },
        {
            "export of new packages",
            ["__test/package1/branch1/"],
            [{"package1", "branch1", rev}],
            fun() -> ok end,
            {ok, [{"package1", "branch1", rev}]}
        },
        {
            "some branch not exported",
            ["__test/package1/branch1/", "__test/package2/no_export/"],
            [{"package1", "branch1", rev}, {"package2", "no_export", rev}],
            fun() -> ok end,
            {ok, [{"package1", "branch1", rev}]}
        },
        {
            "checking previous version cleaned",
            [
                "__test/package1/branch1/some_data/",
                "__test_export/package1/branch1/some_data/"
            ],
            [{"package1", "branch1", rev}],
            fun() -> 
                ?assertEqual(
                    caterpillar_utils:list_packages("__test_export/package1/branch1/"),
                    {ok, []}
                )
            end,
            {ok, [{"package1", "branch1", rev}]}
        }
    ]
]}.



archive_packages_test_() ->
{foreachx,
    fun(Directories) ->
        tty_on(),
        Dirs = Directories++["__test_archive/", "__test_export/"],
        [filelib:ensure_dir(Dir) || Dir <- Dirs],
        #state{archive_root="__test_archive", export_root="__test_export", vcs_plugin=test_vcs_plugin}
    end,
    fun(_, #state{archive_root=AR, export_root=ER}) ->
        [caterpillar_utils:del_dir(D) || D <- [AR, ER]]
    end,
[
    {Setup, fun(_, State) ->
        {Message, fun() ->
            ?assertEqual(
                Result,
                caterpillar_repository:archive_packages(Packages, State)
            ),
            Check()
        end}
    end} || {Message, Setup, Packages, Check, Result} <- [
        {
            "nothing archived",
            [], 
            [],
            fun() -> ok end,
            {error, {archive_packages, "nothing archived"}}
        },
        {
            "package successfuly archived",
            ["__test_export/package/branch/"],
            [{"package", "branch", rev}],
            fun() ->
                ?assertEqual(
                    erl_tar:table("__test_archive/package__ARCHIVE__branch", [compressed]),
                    {ok,["package/branch/"]}
                )
            end,
            {ok, [{"package", "branch", rev}]}
        }
    ]
]}.
