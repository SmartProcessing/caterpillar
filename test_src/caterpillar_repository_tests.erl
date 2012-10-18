-module(caterpillar_repository_tests).

-on_load(tty_off/0).

-include_lib("eunit/include/eunit.hrl").
-include_lib("caterpillar.hrl").
-include_lib("caterpillar_repository_internal.hrl").

-define(ARGS, [
    {vcs_plugin, test_vcs_plugin},
    {repository_db, "repo.db"},
    {work_id_file, "work_id_file"},
    {repository_root, "__test_repo"},
    {export_root, "__test_export"},
    {archive_root, "__archive_root"},
    {notify_root, "__notify_root"}
]).


tty_off() ->
    error_logger:tty(false).


tty_on() ->
    error_logger:tty(true).



start_link_test_() ->
{setup,
    fun() -> ok end,
    fun(_) ->
        ok = caterpillar_repository:stop(),
        file:delete("repo.db"),
        file:delete("work_id_file"),
        [
            caterpillar_utils:del_dir(proplists:get_value(D, ?ARGS)) || 
            D <- [repository_root, archive_root, notify_root, export_root]
        ]
    end,
    fun() ->
        Res = (catch caterpillar_repository:start_link(?ARGS)),
        ?assertMatch({ok, _}, Res),
        {ok, Pid} = Res,
        ?assertEqual(Pid, whereis(caterpillar_repository))
    end
}.



stop_test_() ->
{setup,
    fun() -> {ok, _Pid} = caterpillar_repository:start_link(?ARGS) end,
    fun(_) -> 
        catch (ok = caterpillar_repository:stop()),
        file:delete("repo.db"),
        file:delete("work_id_file"),
        [
            caterpillar_utils:del_dir(proplists:get_value(D, ?ARGS)) || 
            D <- [repository_root, archive_root, notify_root, export_root]
        ]
    end,
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
                    receive Any -> Any after 100 -> timeout end,
                    scan_repository
                )
            end,
            fun() -> 0 end
        },
        {
            "scan_repository received state as parameter with not cancelled timer, timer fires",
            fun(_Ref) ->
                ?assertEqual(
                    receive Any -> Any after 100 -> timeout end,
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
    fun() ->
        [
            {repository_db, "repo.db"},
            {work_id_file, "__test_work_id"},
            {repository_root, "__test_repo"},
            {archive_root, "__test_archive"},
            {export_root, "__test_export"},
            {notify_root, "__test_notify"},
            {scan_interval, 10},
            {vcs_plugin, test_vcs_plugin},
            {vcs_plugin_init, []}
        ]
    end,
    fun(Settings) ->
        [
            caterpillar_utils:del_dir(proplists:get_value(D, Settings))
            || D <- [repository_root, archive_root, export_root, notify_root]
        ],
        [file:delete(F) || F <- ["repo.db", "__test_work_id"]]
    end,
[
    fun(Settings) ->
        {"successful init", fun() ->
            Res = caterpillar_repository:init(Settings),
            ?assertMatch({ok, _}, Res),
            {ok, State} = Res,
            ?assertEqual(State#state.scan_interval, 10000),
            ?assert(is_reference(State#state.scan_timer)),
            ?assertEqual(State#state.archive_root, "__test_archive"),
            ?assertEqual(State#state.notify_root, "__test_notify"),
            ?assertEqual(State#state.export_root, "__test_export"),
            ?assertEqual(State#state.repository_root, "__test_repo"),
            ?assertEqual(State#state.vcs_plugin, test_vcs_plugin),
            ?assertEqual(State#state.vcs_state, state),
            ?assertEqual(ets:info(State#state.ets), ets:info(caterpillar_repository)),
            ?assertEqual(State#state.dets, "repo.db")
        end}
    end,
    fun(Settings) ->
        {"init ok, ensure dets database created", fun() ->
            ?assertMatch(
                {ok, #state{}},
                caterpillar_repository:init(Settings)
            )
        end}
    end
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
            {ok, []}
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
            {ok, []} 
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
        [dets:insert(D, {X, archive, revision, tag, work_id}) || X <- Setup],
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
                {ok, Packages},
                caterpillar_repository:cast_clean_packages(Packages, State)
            ),
            ?assertEqual(
                RecvResult,
                receive Msg -> Msg after 10 -> timeout end
            )
        end}
    end} || {Message, Setup, Packages, RecvResult} <- [
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
            {'$gen_cast',
                {clean_packages,
                    #notify{
                        subject= <<"some packages deleted">>,
                        body= <<"package3/branch3\n">>
                    },
                    [{"package3", "branch3"}]
                }
            }
        }
    ]
]}.



find_modified_packages_test_() ->
{foreachx,
    fun(Setup) ->
        {ok, D} = dets:open_file("test.dets", [{access, read_write}]),
        [dets:insert(D, {X, archive, R, tag, work_id}) || {X, R} <- Setup],
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
            {ok, [
                #package{
                    name="crash", branch="me", 
                    status=error,
                    failed_at=find_modified_packages,
                    reason={'EXIT', some_reason}
                }
            ]}
        }
    ]
]}.


export_packages_test_() ->
{foreachx,
    fun(Directories) ->
        [caterpillar_utils:ensure_dir(Dir) || Dir <- Directories],
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
            ["__test"],
            [],
            fun() -> ok end,
            {error, {export_packages, "nothing exported"}}
        },
        {
            "export of new packages",
            ["__test/package1/branch1"],
            [#package{name="package1", branch="branch1", current_revno=rev}],
            fun() -> ok end,
            {ok, [#package{name="package1", branch="branch1", current_revno=rev}]}
        },
        {
            "some branch not exported",
            ["__test/package1/branch1", "__test/package2/no_export"],
            [
                #package{name="package1", branch="branch1", current_revno=rev},
                #package{name="package2", branch="no_export", current_revno=rev}
            ],
            fun() -> ok end,
            {ok, [
                #package{name="package1", branch="branch1", current_revno=rev},
                #package{
                    name="package2", branch="no_export", status=error,
                    failed_at=export_packages, reason=error, current_revno=rev
                }
            ]}
        },
        {
            "checking package with error status ignored",
            ["__test"],
            [#package{name="ignore me", branch="some branch", status=error}],
            fun() -> ok end,
            {ok, [#package{name="ignore me", branch="some branch", status=error}]}
        },
        {
            "checking previous version cleaned",
            [
                "__test/package1/branch1/some_data",
                "__test_export/package1/branch1/some_data"
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
            "checking packages with bad status ignored",
            [],
            [#package{status=error}],
            fun() -> ok end,
            {ok, [#package{status=error}]}
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
                ?assertEqual(lists:sort(Names), ["dir1", "dir2"])
            end,
            {ok, [#package{
                name="package", branch="branch",
                archive_name="package__ARCHIVE__branch",
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
            "checking packages with error status ignored",
            [#package{name="package1", branch="branch1", status=error}],
            {ok, [#package{name="package1", branch="branch1", status=error}]}
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
            "checking packages with error status ignored",
            [#package{name="package1", branch="branch1", status=error}],
            {ok, [#package{name="package1", branch="branch1", status=error}]}
        },
        { 
            "error while getting changelog",
            [#package{name="package2", branch="branch2"}],
            {ok, [#package{name="package2", branch="branch2", changelog= <<"cant get changelog">>}]}
        }
    ]
]}.


get_tag_test_() ->
{foreach,
    fun() ->
        ok
    end,
    fun(_) ->
        ok
    end,
[
    {Message, fun() ->
        ?assertEqual(
            Result,
            caterpillar_repository:get_tag(Packages, #state{vcs_plugin=test_vcs_plugin})
        )
    end} || {Message, Packages, Result} <- [
        {
            "vcs returns tag",
            [#package{} || _ <- lists:seq(1, 2)],
            {ok, [#package{tag="tag"} || _ <- lists:seq(1, 2)]}
        },
        {
            "vcs crashes on get_tag",
            [#package{}, #package{branch="crash"}],
            {ok, [#package{tag="tag"}, #package{branch="crash"}]}
        }
    ]
]}.


build_result_test_() ->
{foreach,
    fun() ->
        ok
    end,
    fun(_) ->
        ok 
    end,
[
    {Message, fun() ->
        ?assertEqual(
            Result, 
            catch caterpillar_repository:build_result(Packages, state)
        )
    end} || {Message, Packages, Result} <- [
        {
            "one successfuly processed package",
            [#package{
                name="package", branch="branch", archive_name="archive",
                diff= <<"diff">>, changelog= <<"changelog">>
            }],
            {ok, #scan_pipe_result{
                notify=#notify{
                    subject = <<>>,
                    body = <<"\n\npackage/branch\nchangelog\nDiff contains 4 bytes\ndiff\n">>
                },
                packages = [#package{
                    name="package", branch="branch", archive_name="archive",
                    diff= <<>>, changelog= <<>>
                }],
                archives = [
                    #archive{name="package", branch="branch", archive_name="archive"}
                ]
            }}
        },
        {
            "one failed to process package",
            [#package{
                name="p", branch="b", failed_at=somewhere, reason=some_error, status=error
            }],
            {ok,
                #scan_pipe_result{
                notify=#notify{
                    subject = <<>>,
                    body = <<"\n\np/b failed at somewhere\nsome_error\n">>
                },
                packages = [#package{
                    name="p", branch="b", failed_at=somewhere, reason=some_error, status=error
                }],
                archives = []
            }}
        },
        {
            "one package failed, second package processed",
            [
                #package{
                    name="package", branch="branch", archive_name="archive",
                    diff= <<"diff">>, changelog= <<"changelog">>
                },
                #package{
                    name="p", branch="b", failed_at=somewhere, reason=some_error, status=error
                }
            ],
            {ok,
                #scan_pipe_result{
                notify=#notify{
                    subject = <<>>,
                    body = <<
                        "\n\npackage/branch\nchangelog\nDiff contains 4 bytes\ndiff\n\n\n"
                        "p/b failed at somewhere\nsome_error\n"
                    >>
                },
                packages = [
                    #package{
                        name="package", branch="branch", archive_name="archive",
                        diff= <<>>, changelog= <<>>
                    },
                    #package{
                        name="p", branch="b", failed_at=somewhere, reason=some_error, status=error
                    }
                ],
                archives = [
                    #archive{name="package", branch="branch", archive_name="archive"}
                ]
            }}
        }
    ]
]}.


limit_output_test_() ->
{foreach,
    fun() -> ok end,
[
    {Message, fun() ->
        ?assertEqual(
            Result,
            caterpillar_repository:limit_output(Bin, Size)
        )
    end} || {Message, {Bin, Size}, Result} <- [
        {
            "bin not parted",
            {<<"123">>, 4},
            {3, <<"123">>}
        },
        {
            "bin parted",
            {<<"1234">>, 3},
            {4, <<"123...">>}
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
                dets:insert(D, {{Name, Branch}, archive, last_revision, tag, work_id}) 
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
                lists:sort([{Package, archive, last_revision, tag, work_id} || Package <- PackageList]),
                lists:sort(dets:select(D, [{'$1', [], ['$1']}]))
            ),
            ?assertEqual(
                {ok, lists:usort([N || {N, _} <- PackageList])},
                caterpillar_utils:list_packages(ER)
            ),
            caterpillar_repository:clean_packages(State, CleanPackages),
            ?assertEqual(
                lists:sort(
                    [{{N, B}, archive, last_revision, tag, work_id} || #package{name=N, branch=B} <- AfterClean]
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
            [{"p1", "b1"}],
            [#package{name="p2", branch="b2"}]
        },
        {
            "one package got few branches, one of them cleaned",
            [#package{name=N, branch=B} || {N, B} <- [{"p1", "b1"}, {"p1", "b2"}, {"p2", "b2"}]],
            [{"p1", "b1"}],
            [#package{name="p1", branch="b2"}, #package{name="p2", branch="b2"}]
        },
        {
            "one package got few branches, both of them cleaned",
            [#package{name=N, branch=B} || {N, B} <- [{"p1", "b1"}, {"p1", "b2"}, {"p2", "b2"}]],
            [{"p1", "b1"}, {"p1", "b2"}],
            [#package{name="p2", branch="b2"}]
        },
        {
            "all packages cleaned",
            [#package{name=N, branch=B} || {N, B} <- [{"p1", "b1"}, {"p1", "b2"}, {"p2", "b2"}]],
            [{"p1", "b1"}, {"p1", "b2"}, {"p2", "b2"}],
            []
        },
        {
            "no such packages, clean_packages should not crash",
            [],
            [{"p1", "b1"}, {"p1", "b2"}, {"p2", "b2"}],
            []
        }            
    ]
]}.


select_archives_by_work_id_test_() ->
{foreachx,
    fun(Archives) -> 
        {ok, D} = dets:open_file("__test.dets", [{access, read_write}]),
        dets:insert(D, Archives),
        #state{dets=D}
    end,
    fun(_, #state{dets=D}) ->
        dets:close(D),
        file:delete(D)
    end,
[
    {Archives, fun(_, State) ->
        {Message, fun() ->
            ?assertEqual(
                Result, 
                lists:sort(
                    caterpillar_repository:select_archives_by_work_id(State, WorkId)
                )
            )
        end}
    end} || {Message, Archives, WorkId, Result} <- [
        {
            "dets empty",
            [],
            1,
            []
        },
        {
            "few archives, nothing selected",
            [
                {{package1, branch1}, archive_name1, last_revno, tag, 1},
                {{package2, branch2}, archive_name1, last_revno, tag, 1}
            ],
            1,
            []
        },
        {
            "few archives, one of them selected",
            [
                {{package1, branch1}, archive_name1, last_revno, tag, 2},
                {{package2, branch2}, archive_name1, last_revno, tag, 1}
            ],
            1,
            [#archive{name=package1, branch=branch1, archive_name=archive_name1, tag=tag}]
        },
        {
            "few archives, all of them selected",
            [
                {{package1, branch1}, archive_name1, last_revno, tag, 2},
                {{package2, branch2}, archive_name2, last_revno, tag, 2}
            ],
            1,
            [
                #archive{name=package1, branch=branch1, archive_name=archive_name1, tag=tag},
                #archive{name=package2, branch=branch2, archive_name=archive_name2, tag=tag}
            ]
        }
        
    ]
]}.


async_notify_test_() ->
{foreachx,
    fun(Data) -> 
        NR = "__test_notify_root",
        caterpillar_utils:ensure_dir(NR),
        lists:foreach(
            fun({Name, D}) ->
                Filename = filename:join(NR, Name),
                file:write_file(Filename, term_to_binary(D))
            end,
            Data
        ),
        #state{notify_root=NR}
    end,
    fun(_, #state{notify_root=NR}) ->
        caterpillar_utils:del_dir(NR)
    end,
[
    {Data, fun(_, State) ->
        {Message, fun() ->
            Setup(),
            ?assertEqual(
                Result,
                catch caterpillar_repository:async_notify(State)
            ),
            Check(State)
        end}
    end} || {Message, Data, Setup, Check, Result} <- [
        {
            "nothing in directory",
            [],
            fun() -> ok end,
            fun(_) -> ok end,
            ok
        },
        {
            "some files exists, but no event services available",
            [{"test", <<"d">>}],
            fun() -> ok end,
            fun(#state{notify_root=NR}) ->
                ?assertEqual(
                    {ok, ["test"]},
                    file:list_dir(NR)
                ),
                {ok, Data} = file:read_file(filename:join(NR, "test")),
                ?assertEqual(
                    <<"d">>,
                    binary_to_term(Data)
                )
            end,
            ok
        },
        {
            "some files exists, event service available",
            [{"test", <<"d">>}],
            fun() -> 
                spawn(fun() ->
                    global:register_name(caterpillar_event, self()),
                    receive {_, From, _} ->
                        gen_server:reply(From, ok)
                    after 500 ->
                        timeout
                    end
                end),
                timer:sleep(5)
            end,
            fun(#state{notify_root=NR}) ->
                ?assertEqual(
                    {ok, []},
                    file:list_dir(NR)
                )
            end,
            ok
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
            ?assertEqual(
                ok,
                catch caterpillar_repository:notify(State, Notify)
            ),
            Check(State)
        end} 
    end || {Message, Mock, Notify, Check} <- [
        {
            "new_packages, catepillar event not available",
            fun() -> ok end,
            #notify{subject= <<"some change">>, body= <<"body">>},
            fun(#state{notify_root=NR}) ->
                Listing = file:list_dir(NR),
                ?assertMatch(
                    {ok, _},
                    Listing
                ),
                {ok, [Filename|_]} = Listing,
                {ok, Data} = file:read_file(filename:join(NR, Filename)),
                ?assertEqual(
                    binary_to_term(Data),
                    #notify{subject= <<"some change">>, body= <<"body">>}
                )
            end
        },
        {
            "clean_packages, caterpillar event not available",
            fun() -> ok end,
            #notify{subject = <<"notify">>, body = <<"notify">>},
            fun(#state{notify_root=NR}) ->
                Listing = file:list_dir(NR),
                ?assertMatch(
                    {ok, _},
                    Listing
                ),
                {ok, [Filename|_]} = Listing,
                {ok, Data} = file:read_file(filename:join(NR, Filename)),
                ?assertEqual(
                    binary_to_term(Data),
                    #notify{subject = <<"notify">>, body = <<"notify">>}
                )
            end
        }
    ]
]}.
        


%%
%% gen_server callback tests
%%


handle_call_changes_test_() ->
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
            Return = (catch caterpillar_repository:handle_call(Request, from, State)),
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
            "changes test",
            {changes, #scan_pipe_result{packages=[#package{}], archives=[#archive{}], notify=#notify{}}},
            #state{work_id_file="test_work_id_file", dets="test_d", work_id=1, ets=ets:new(t, [])},
            fun(#state{work_id_file=BIF, dets=D}) -> 
                ?assertEqual(
                    caterpillar_utils:read_work_id(BIF),
                    2
                ),
                ?assertEqual(
                    [{{undefined, undefined}, undefined, undefined, undefined, 2}],
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
            [{{package, branch}, archive, current_revno, tag, work_id}],
            [{package, branch}]
        },
        {
            "few packages in dets",
            [{{Package, Branch}, archive, current_revno, tag, work_id} || {Package, Branch} <- [{p, b}, {pp, bb}]],
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
                        {'$gen_call', _, {changes, #scan_pipe_result{
                            notify=#notify{
                                subject = <<>>,
                                body = <<
                                    "\n\npackage1/branch1\nbranch1 changelog\nDiff contains 12 bytes"
                                    "\nbranch1 diff\n"
                            >>},
                            packages = [
                                #package{name="package1", branch="branch1", current_revno=1,
                                    archive_name="package1__ARCHIVE__branch1",
                                    diff= <<>>, changelog= <<>>
                                }
                            ],
                            archives = [
                                #archive{
                                    name="package1", branch="branch1",
                                    archive_name="package1__ARCHIVE__branch1"
                                }
                            ]
                        }}}
                    ]},
                    process_info(self(), messages)
                )
            end
        }            
    ]
]}.


handle_call_get_archive_test_() ->
{foreachx,
    fun(Archives) ->
        AR = "__test_archive",
        caterpillar_utils:ensure_dir(AR),
        {ok, D} = dets:open_file("__test.dets", [{access, read_write}]),
        lists:foreach(
            fun({Package, Branch, ArchiveName, ArchiveData}) ->
                file:write_file(filename:join(AR, ArchiveName), ArchiveData),
                dets:insert(D, {{Package, Branch}, ArchiveName, last_revno, tag, work_id})
            end,
            Archives
        ),
        #state{archive_root=AR, dets=D}
    end,
    fun(_, #state{archive_root=AR, dets=D}) ->
        dets:close(D), file:delete(D),
        caterpillar_utils:del_dir(AR)
    end,
[
    {Archives, fun(_, State) ->
        {Message, fun() ->
            ?assertEqual(
                {noreply, State},
                caterpillar_repository:handle_call({get_archive, Archive(State)}, {self(), ref}, State)
            ),
            Check(State)
        end}
    end} || {Message, Archives, Archive, Check} <- [
        {
            "no archive",
            [], 
            fun(_) -> #archive{name="test", branch="branch"} end,
            fun(_) ->
                ?assertEqual(
                    {ref, {error, get_archive}},
                    receive Msg -> Msg after 100 -> timeout end
                )
            end
        },
        {
            "archive exists and copied",
            [{"test", "branch", "test_branch", "archive_data"}],
            fun(#state{archive_root=AR}) -> 
                {ok, Fd} = file:open(filename:join(AR, "copy_here"), [write]),
                #archive{name="test", branch="branch", fd=Fd}
            end,
            fun(#state{archive_root=AR}) ->
                ?assertEqual(
                    {ref, ok},
                    receive Msg -> Msg after 100 -> timeout end
                ),
                ?assertEqual(
                    {ok, <<"archive_data">>},
                    file:read_file(filename:join(AR, "copy_here"))
                )
            end
        }
    ]

]}.



handle_call_get_archives_test_() ->
{foreachx,
    fun(Archives) ->
        {ok, D} = dets:open_file("__test_dets", [{access, read_write}]),
        dets:insert(D, Archives),
        #state{dets=D, work_id=3}
    end,
    fun(_, #state{dets=D}) ->
        dets:close(D),
        file:delete(D)
    end,
[
    {Archives, fun(_, State) ->
        {Message, fun() ->
            caterpillar_repository:handle_call({get_archives, WorkId}, {self(), ref}, State),
            ?assertEqual(
                Result,
                receive Msg -> Msg after 100 -> timeout end
            )
        end}
    end} || {Message, Archives, WorkId, Result} <- [
        {
            "no archives",
            [],
            2,
            {ref, {ok, {changes, 3, []}}}
        },
        {
            "archive avaiable, but not pushed",
            [{{package, branch}, achive, last_revno, tag, 2}],
            2,
            {ref, {ok, {changes, 3, []}}}
        },
        {
            "archive avaiable and pushed",
            [{{package, branch}, archive, last_revno, tag, 2}],
            1,
            {ref, {ok, {changes, 3, [#archive{name=package, branch=branch, tag=tag, archive_name=archive}]}}}
        }
    ]
]}.
