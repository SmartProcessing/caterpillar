-module(caterpillar_repository_tests).

-on_load(tty_off/0).

-include_lib("eunit/include/eunit.hrl").
-include_lib("caterpillar.hrl").
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
    fun() -> {ok, _Pid} = caterpillar_repository:start_link(?ARGS) end,
    fun(_) -> catch (ok = caterpillar_repository:stop()), file:delete("repo.db") end,
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
            {ok, [
                #package{name="package1"},
                #package{name="package2"}
            ]}
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
            [#package{name=X} || X <- ["package1", "package2"]],
            {error, {get_branches, "no branches in repositories"}}
        },
        {
            "one branch in one repo",
            ["__test/package1/branch1/", "__test/package2/"],
            [#package{name="package1"}],
            {ok, [#package{name="package1", branch="branch1"}]}
        },
        {
            "few branches in different repos",
            ["__test/package1/branch1/", "__test/package2/branch2/"],
            [#package{name=X} || X <- ["package1", "package2"]],
            {ok, [#package{name=Name, branch=Branch} || {Name, Branch} <- [
                {"package1", "branch1"}, {"package2", "branch2"}
            ]]}
        },
        {
            "plugin exits on branch check",
            [
                "__test/package1/exit/", "__test/package1/branch1/",
                "__test/package2/throw/", "__test/package2/branch2/"
            ],
            [#package{name=X} || X <- ["package1", "package2"]],
            {ok, [#package{name=Name, branch=Branch} || {Name, Branch} <- [
                {"package1", "branch1"}, {"package2", "branch2"}
            ]]}
        }
    ]
]}.


cast_clean_packages_test_() ->
{foreachx,
    fun(Setup) ->
        {ok, D} = dets:open_file("test.dets", [{access, read_write}]),
        [dets:insert(D, {X, archive, revision, work_id}) || X <- Setup],
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
                caterpillar_repository:cast_clean_packages(Branches, State)
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
            [#package{name=Name, branch=Branch} || {Name, Branch} <- [
                {"package1", "branch1"}, {"package2", "branch2"}
            ]],
            timeout
        },
        {
            "something in dets, but nothing to delete",
            [{"package1", "branch1"}],
            [#package{name=Name, branch=Branch} || {Name, Branch} <- [
                {"package1", "branch1"}, {"package2", "branch2"}
            ]],
            timeout
        },
        {
            "deleting missing branches n packages",
            [{"package1", "branch1"}, {"package3", "branch3"}],
            [#package{name=Name, branch=Branch} || {Name, Branch} <- [
                {"package1", "branch1"}, {"package2", "branch2"}
            ]],
            {'$gen_cast', {clean_packages, [#package{name="package3", branch="branch3"}]}}
        }
    ]
]}.



find_modified_packages_test_() ->
{foreachx,
    fun(Setup) ->
        {ok, D} = dets:open_file("test.dets", [{access, read_write}]),
        [dets:insert(D, {X, archive, R, work_id}) || {X, R} <- Setup],
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
                caterpillar_repository:find_modified_packages(Packages, State)
            )
        end}
    end} || {Message, Setup, Packages, Result} <- [
        {
            "new package (package2/branch2)",
            [{{"package1", "branch1"}, 1}],
            [#package{name=Name, branch=Branch} || {Name, Branch} <- [
                {"package1", "branch1"}, {"package2", "branch2"}
            ]],
            {ok, [#package{name="package2", branch="branch2", current_revno=1}]}
        },
        {
            "both packages not modified",
            [{{"package1", "branch1"}, 1}, {{"package2", "branch2"}, 1}],
            [#package{name=Name, branch=Branch} || {Name, Branch} <- [
                {"package1", "branch1"}, {"package2", "branch2"}
            ]],
            {error,{find_modified_packages,"no packages modified"}}
        },
        {
            "both packages modified",
            [{{"package1", "branch1"}, 10}, {{"package2", "branch2"}, 10}],
            [#package{name=Name, branch=Branch} || {Name, Branch} <- [
                {"package1", "branch1"}, {"package2", "branch2"}
            ]],
            {ok, [
                #package{name="package1", branch="branch1", current_revno=1, old_revno=10},
                #package{name="package2", branch="branch2", current_revno=1, old_revno=10}
            ]}
        },
        {
            "plugin returns bad response",
            [],
            [#package{name="crash", branch="me"}],
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
            [#package{name="package1", branch="branch1", current_revno=rev}],
            fun() -> ok end,
            {ok, [#package{name="package1", branch="branch1", current_revno=rev}]}
        },
        {
            "some branch not exported",
            ["__test/package1/branch1/", "__test/package2/no_export/"],
            [
                #package{name="package1", branch="branch1", current_revno=rev},
                #package{name="package2", branch="no_export", current_revno=rev}
            ],
            fun() -> ok end,
            {ok, [#package{name="package1", branch="branch1", current_revno=rev}]}
        },
        {
            "checking previous version cleaned",
            [
                "__test/package1/branch1/some_data/",
                "__test_export/package1/branch1/some_data/"
            ],
            [#package{name="package1", branch="branch1", current_revno=rev}],
            fun() -> 
                ?assertEqual(
                    caterpillar_utils:list_packages("__test_export/package1/branch1/"),
                    {ok, []}
                )
            end,
            {ok, [#package{name="package1", branch="branch1", current_revno=rev}]}
        }
    ]
]}.



archive_packages_test_() ->
{foreachx,
    fun(Directories) ->
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
            [
                "__test_export/package/branch/dir1/",
                "__test_export/package/branch/dir2/"
                
            ],
            [#package{name="package", branch="branch", current_revno=rev}],
            fun() ->
                {ok, Names} = erl_tar:table("__test_archive/package__ARCHIVE__branch", [compressed]),
                ?assertEqual(lists:sort(Names), ["package/branch/dir1", "package/branch/dir2"])
            end,
            {ok, [#package{
                name="package", branch="branch",
                archive="package__ARCHIVE__branch",
                current_revno=rev
            }]}
        }
    ]
]}.



get_diff_test_() ->
{foreach,
    fun() -> #state{vcs_plugin=test_vcs_plugin, repository_root=""} end,
[
    fun(State) ->
        {Message, fun() ->
            ?assertEqual(
                Result,
                caterpillar_repository:get_diff(Packages, State)
            )
        end}
    end || {Message, Packages, Result} <- [
        {
            "plugin returns valid response",
            [#package{name="package1", branch="branch1"}],
            {ok, [#package{name="package1", branch="branch1", diff= <<"branch1 diff">>}]}
        },
        { 
            "error while getting diff",
            [#package{name="package2", branch="branch2"}],
            {ok, [#package{name="package2", branch="branch2", diff= <<"cant get diff">>}]}
        }
    ]
]}.



get_changelog_test_() ->
{foreach,
    fun() -> #state{vcs_plugin=test_vcs_plugin, repository_root=""} end,
[
    fun(State) ->
        {Message, fun() ->
            ?assertEqual(
                Result,
                caterpillar_repository:get_changelog(Packages, State)
            )
        end}
    end || {Message, Packages, Result} <- [
        {
            "plugin returns valid response",
            [#package{name="package1", branch="branch1"}],
            {ok, [#package{name="package1", branch="branch1", changelog= <<"branch1 changelog">>}]}
        },
        { 
            "error while getting changelog",
            [#package{name="package2", branch="branch2"}],
            {ok, [#package{name="package2", branch="branch2", changelog= <<"cant get changelog">>}]}
        }
    ]
]}.


clean_packages_test_() ->
{foreachx,
    fun(Packages) ->
        ER = "__test_export",
        AR = "__test_archive",
        [caterpillar_utils:ensure_dir(Dir) || Dir <- [ER, AR]],
        {ok, D} = dets:open_file("test.dets", [{access, read_write}]),
        [
            begin
                caterpillar_utils:ensure_dir(filename:join([ER, Name, Branch])),
                Archive = filename:join(AR, caterpillar_utils:package_to_archive(Name, Branch)),
                filelib:ensure_dir(Archive),
                file:write_file(Archive, <<"h">>),
                dets:insert(D, {{Name, Branch}, archive, last_revision, work_id}) 
            end || #package{name=Name, branch=Branch} <- Packages
        ],
        #state{dets=D, export_root=ER, archive_root=AR, vcs_plugin=test_vcs_plugin}
    end,
    fun(_Packages, #state{dets=D, archive_root=AR, export_root=ER}) ->
        dets:close(D),
        file:delete(D),
        [caterpillar_utils:del_dir(Dir) || Dir <- [AR, ER]]
    end,
[
    {Packages, fun(_, #state{dets=D, export_root=ER}=State) ->
        {Message, fun() ->
            PackageList = [{N, B} || #package{name=N, branch=B} <- Packages],
            ?assertEqual(
                lists:sort([{Package, archive, last_revision, work_id} || Package <- PackageList]),
                lists:sort(dets:select(D, [{'$1', [], ['$1']}]))
            ),
            ?assertEqual(
                {ok, lists:usort([N || {N, _} <- PackageList])},
                caterpillar_utils:list_packages(ER)
            ),
            caterpillar_repository:clean_packages(State, CleanPackages),
            ?assertEqual(
                lists:sort(
                    [{{N, B}, archive, last_revision, work_id} || #package{name=N, branch=B} <- AfterClean]
                ),
                lists:sort(dets:select(D, [{'$1', [], ['$1']}]))
            ),
            ?assertEqual(
                {ok, [N || #package{name=N} <- AfterClean]},
                caterpillar_utils:list_packages(ER)
            )
        end}
    end} || {Message, Packages, CleanPackages, AfterClean} <- [
        {
            "nothing cleaned",
            [#package{name="package", branch="branch"}],
            [],
            [#package{name="package", branch="branch"}]
        },
        {
            "one package cleaned",
            [#package{name=N, branch=B} || {N, B} <- [{"p1", "b1"}, {"p2", "b2"}]],
            [#package{name="p1", branch="b1"}],
            [#package{name="p2", branch="b2"}]
        },
        {
            "one package got few branches, one of them cleaned",
            [#package{name=N, branch=B} || {N, B} <- [{"p1", "b1"}, {"p1", "b2"}, {"p2", "b2"}]],
            [#package{name="p1", branch="b1"}],
            [#package{name="p1", branch="b2"}, #package{name="p2", branch="b2"}]
        },
        {
            "one package got few branches, both of them cleaned",
            [#package{name=N, branch=B} || {N, B} <- [{"p1", "b1"}, {"p1", "b2"}, {"p2", "b2"}]],
            [#package{name="p1", branch="b1"}, #package{name="p1", branch="b2"}],
            [#package{name="p2", branch="b2"}]
        },
        {
            "all packages cleaned",
            [#package{name=N, branch=B} || {N, B} <- [{"p1", "b1"}, {"p1", "b2"}, {"p2", "b2"}]],
            [#package{name=N, branch=B} || {N, B} <- [{"p1", "b1"}, {"p1", "b2"}, {"p2", "b2"}]],
            []
        },
        {
            "no such packages, clean_packages should not crash",
            [],
            [#package{name=N, branch=B} || {N, B} <- [{"p1", "b1"}, {"p1", "b2"}, {"p2", "b2"}]],
            []
        }            
    ]
]}.



notify_test_() ->
{foreach,
    fun() ->
        NR = "__test_notify",
        caterpillar_utils:ensure_dir(NR),
        #state{notify_root=NR, work_id=1}
    end,
    fun(#state{notify_root=NR}) ->
        caterpillar_utils:del_dir(NR)
    end,
[
    fun(State) ->
        {Message, fun() ->
            Mock(),
            caterpillar_repository:notify(Type, State, Packages),
            Check(State)
        end} 
    end || {Message, Mock, {Type, Packages}, Check} <- [
        {
            "new_packages, catepillar event not available",
            fun() -> ok end,
            {new_packages, [#package{name="p", branch="b"}]},
            fun(#state{notify_root=NR}) ->
                ?assertEqual(
                    {ok, ["1"]},
                    file:list_dir(NR)
                ),
                {ok, Data} = file:read_file(filename:join(NR, "1")),
                ?assertEqual(
                    binary_to_term(Data),
                    {new_packages, 1, [#package{name="p", branch="b"}]}
                )
            end
        },
        {
            "clean_packages, caterpillar event not available",
            fun() -> ok end,
            {clean_packages, [#package{name="p", branch="b"}]},
            fun(#state{notify_root=NR}) ->
                ?assertEqual(
                    {ok, ["1"]},
                    file:list_dir(NR)
                ),
                {ok, Data} = file:read_file(filename:join(NR, "1")),
                ?assertEqual(
                    binary_to_term(Data),
                    {clean_packages, 1, [#package{name="p", branch="b"}]}
                )
            end
        }
    ]
]}.
        


%%
%% gen_server callback tests
%%


handle_call_new_packages_test_() ->
{foreachx,
    fun(#state{dets=D}=State) ->
        {ok, D} = dets:open_file(D, [{access, read_write}]),
        State#state{work_id=1}
    end,
    fun(_, #state{work_id_file=BIF, dets=D}) ->
        dets:close(D),
        [file:delete(F) || F <- [D, BIF]]
    end,
[
    {Setup, fun(_, State) ->
        {Message, fun() ->
            Return = caterpillar_repository:handle_call(Request, from, State),
            ?assertMatch(
                {reply, ok, _},
                Return
            ),
            {_, _, NewState} = Return,
            ?assertEqual(
                Result,
                NewState#state{ets=undefined}
            ),
            Check(State)
        end}
    end} || {Message, Request, Setup, Check, Result} <- [
        {
            "new_packages test",
            {new_packages, [#package{}]},
            #state{work_id_file="test_work_id_file", dets="test_d", work_id=1, ets=ets:new(t, [])},
            fun(#state{work_id_file=BIF, dets=D}) -> 
                ?assertEqual(
                    caterpillar_utils:read_work_id(BIF),
                    2
                ),
                ?assertEqual(
                    [{{undefined, undefined}, undefined, undefined, 2}],
                    dets:select(D, [{'$1', [], ['$1']}])
                )
            end,
            #state{work_id_file="test_work_id_file", dets="test_d", work_id=2}
        }
    ]
]}.


handle_call_get_packages_test_() ->
{foreachx,
    fun(Packages) -> 
        {ok, D} = dets:open_file("test_dets", [{access, read_write}]),
        dets:insert(D, Packages),
        #state{dets=D}
    end,
    fun(_, #state{dets=D}) ->
        dets:close(D),
        file:delete(D)
    end,
[
    {Setup, fun(_, State) ->
        {Message, fun() ->
            Return = caterpillar_repository:handle_call(get_packages, from, State),
            ?assertMatch({_, _, _}, Return),
            {_, Response, _} = Return,
            ?assertEqual(lists:sort(Response), Result)
        end}
    end} || {Message, Setup, Result} <- [
        {
            "no packages available",
            [],
            []
        },
        {
            "one package in dets",
            [{{package, branch}, archive, current_revno, work_id}],
            [{package, branch}]
        },
        {
            "few packages in dets",
            [{{Package, Branch}, archive, current_revno, work_id} || {Package, Branch} <- [{p, b}, {pp, bb}]],
            [{p, b}, {pp, bb}]
        }
    ]
]}.


handle_info_scan_repository_test_() ->
{foreachx,
    fun(Packages) -> 
        RR = "__test",
        AR = "__test_archive",
        ER = "__test_export",
        {ok, Dets} = dets:open_file("__test_dets.db", [{access, read_write}]),
        [caterpillar_utils:ensure_dir(D) || D <- [RR, AR, ER]],
        [caterpillar_utils:ensure_dir(filename:join(RR, Package)) || Package <- Packages],
        #state{
            repository_root=RR, vcs_plugin=test_vcs_plugin, dets=Dets,
            export_root=ER, archive_root=AR
        }
    end,
    fun(_Packages, #state{dets=Dets, repository_root=RR, export_root=ER, archive_root=AR}) ->
        dets:close(Dets),
        file:delete(Dets),
        [caterpillar_utils:del_dir(D) || D <- [RR, ER, AR]]
    end,
[   
    {Packages, fun(_, State) ->
        {Message, fun() ->
            register(caterpillar_repository, self()),
            ?assertMatch(
                {noreply, #state{}},
                caterpillar_repository:handle_info(scan_repository, State)
            ),
            Check()
        end}
    end} || {Message, Packages, Check} <- [
        {
            "no packages", [], fun() -> ok end
        },
        {
            "some packages, without branch",
            ["sleep"],
            fun() ->
                timer:sleep(2),
                Pid = whereis(scan_pipe_caterpillar_repository),
                ?assert(is_pid(Pid) andalso is_process_alive(Pid)),
                timer:sleep(10),
                ?assertEqual(
                    process_info(self(), messages),
                    {messages, []}
                )
            end
        },
        {
            "some packages with some branches",
            ["sleep", "package1/branch1"],
            fun() ->
                timer:sleep(1),
                Pid = whereis(scan_pipe_caterpillar_repository),
                ?assert(is_pid(Pid) andalso is_process_alive(Pid)),
                timer:sleep(15),
                ?assertMatch(
                    {messages, [
                        {'$gen_call', _, {new_packages, [
                            #package{name="package1", branch="branch1", current_revno=1,
                                archive="package1__ARCHIVE__branch1",
                                diff= <<"branch1 diff">>, changelog= <<"branch1 changelog">>
                            }
                        ]}}
                    ]},
                    process_info(self(), messages)
                )
            end
        }            
    ]
]}.



%---


push_work_testz() ->
    ?assertEqual(
        ok,
        caterpillar_repository:push_work(#state{})
    ).


get_work_id_test_() ->
{foreach,
    fun() -> ok end,
[
    {Message, fun() ->
        ?assertEqual(
            Result,
            caterpillar_repository:get_work_id('_', State)
        )
    end} || {Message, State, Result} <- [
        {
            "pid not alive",
            #work_state{pid=spawn(fun() -> ok end)},
            {error, {get_work_id, bad_response}}
        },
        {
            "pid alive, bad return",
            #work_state{
                pid=spawn(fun() -> receive {_, From, _} -> gen_server:reply(From, 'wat?') end end)
            },
            {error, {get_work_id, bad_response}}
        },
        {
            "pid alive, valid response",
            #work_state{
                pid=spawn(fun() -> receive {_, From, _} -> gen_server:reply(From, {ok, 1}) end end)
            },
            {ok, 1}
        }
    ]
]}.


select_archives_test_() ->
{foreachx,
    fun(Archives) ->
        {ok, Dets} = dets:open_file("test_dets", [{access, read_write}]),
        AR = "__test_archive",
        [
            begin
                dets:insert(Dets, {{Package, Name}, Archive, current_revno, BuildId}),
                ArchiveName = filename:join(AR, Archive),
                filelib:ensure_dir(ArchiveName),
                file:write_file(ArchiveName, <<>>)
            end || {Package, Name, Archive, BuildId} <- Archives
        ],
        #work_state{dets=Dets, archive_root=AR}
    end,
    fun(_, #work_state{dets=Dets, archive_root=AR}) ->
        dets:close(Dets), file:delete(Dets),
        caterpillar_utils:del_dir(AR)
    end,
[
    {Setup, fun(_, State) ->
        {Message, fun() ->
            Check(State)
        end}
    end} || {Message, Setup, Check} <- [
        {
            "no archives for work",
            [],
            fun(State) ->
                ?assertEqual(
                    {error, {select_archives, no_archives}},
                    caterpillar_repository:select_archives(1, State)
                )
            end
        },
        {
            "some archives selected",
            [
                {"p1", "b1", "a1", 1},
                {"p2", "b2", "a2", 2}
            ],
            fun(State) ->
                Result = caterpillar_repository:select_archives(1, State),
                ?assertMatch(
                    {ok, [#archive{name="p2", branch="b2"}]},
                    Result
                ),
                {_, [{_, _, _, Pid}]} = Result,
                ?assertEqual(
                    {ok, "__test_archive/a2"},
                    file:pid2name(Pid)
                )
            end
        },
        {
            "worker got latest work_id",
            [
                {"p1", "b1", "a1", 2},
                {"p2", "b2", "a2", 2}
            ],
            fun(State) ->
                ?assertMatch(
                    {error, {select_archives, no_archives}},
                    caterpillar_repository:select_archives(2, State)
                )
            end
        },
        {
            "both files r new to worker",
            [
                {"p1", "b1", "a1", 2},
                {"p2", "b2", "a2", 2}
            ],
            fun(State) ->
                Result = caterpillar_repository:select_archives(1, State),
                ?assertMatch(
                    {ok, [#archive{name="p1", branch="b1"}, #archive{name="p2", branch="b2"}]},
                    Result
                ),
                {ok, Archives} = Result,
                ?assertEqual(
                    [{ok, "__test_archive/a1"}, {ok, "__test_archive/a2"}],
                    lists:sort([file:pid2name(Pid) || {_, _, _, Pid} <- Archives])
                )
            end
        }
    ]
]}.


work_query_test_() ->
{foreach,
    fun() -> ok end,
[
    {Message, fun() -> 
        ?assertEqual(
            Result,
            caterpillar_repository:work_query([], State)
        )
    end} || {Message, State, Result} <- [
        {
            "worker pid down",
            #work_state{pid = spawn(fun() -> ok end)},
            {error, work_query}
        },
        {
            "worker pid after request receive",
            #work_state{pid = spawn(fun() -> ok end)},
            {error, work_query}
        },
        {
            "worker pid returns bad response",
            #work_state{pid = spawn(fun() -> receive {_, F, _} -> gen_server:reply(F, 'wat?') end end)},
            {error, work_query}
        },
        {
            "worker pid returns valid response",
            #work_state{pid = spawn(fun() -> receive {_, F, _} -> gen_server:reply(F, {ok, done}) end end)},
            {ok, done}
        }
    ]
]}.


deploy_test_() ->
{foreach,
    fun() -> ok end,
[
    {Message, fun() ->
        global:register_name(caterpillar_router, self()),
        ?assertEqual(
            {ok, done},
            caterpillar_repository:deploy(Deploy, State)
        ),
        timer:sleep(1),
        ?assertEqual(
            {messages, Messages},
            process_info(self(), messages)
        )
    end} || {Message, Deploy, State, Messages} <- [
        {"no deploy", no_deploy, #work_state{}, []},
        {"deploy", some_deploy, #work_state{}, [{'$gen_cast', {deploy, some_deploy}}]}
    ]
]}.
