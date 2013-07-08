-module(caterpillar_deploy_tests).


-on_load(tty_off/0).


-include_lib("eunit/include/eunit.hrl").
-include_lib("caterpillar.hrl").
-include_lib("caterpillar_deploy_internal.hrl").


tty_off() ->
    error_logger:tty(false).


tty_on() ->
    error_logger:tty(true).




register_as_service_test() ->
    caterpillar_deploy:register_as_service(0),
    ?assertEqual(register_as_service, caterpillar_test_support:recv(50)).


init_ets_test_() ->
{foreachx,
    fun(Idents) -> {Idents, "./test/test_deploy"} end,
    fun(_, {_Idents, Path}) ->
        caterpillar_utils:del_dir(Path)
    end,
[
    {Setup, fun(_, {Idents, DeployPath}) ->
        {Message, fun() ->
            Ets = (catch caterpillar_deploy:init_ets(Idents, DeployPath)),
            ?assertEqual(Result, lists:sort(ets:tab2list(Ets)))
        end}
    end} || {Message, Setup, Result} <- [
        {
            "no paths",
            [],
            []
        },
        {
            "some idents in cfg, but without default values",
            [{os_type, [{amd64, [{branch, "__test_amd"}]}]}],
            [{{os_type, branch, amd64}, "./test/test_deploy/__test_amd"}]
        }
    ]
]}.



run_pre_deploy_test() ->
    ?assertEqual({ok, result}, caterpillar_deploy:run_pre_deploy(result, state)).


find_deploy_paths_test_() ->
{foreachx,
    fun(Idents) -> 
        Ets = caterpillar_deploy:init_ets(Idents),
        #state{ets=Ets}
    end,
    fun(Idents, #state{ets=Ets}) ->
        ets:delete(Ets),
        [caterpillar_utils:del_dir(Path) || {Ident, Archs} <- Idents, {Arch, Branches} <- Archs, {Branch, Path} <- Branches]
    end,
[
    {Idents, fun(_, State) ->
        {Message, fun() ->
            R = (catch caterpillar_deploy:find_deploy_paths(Deploy, State)),
            ?assertMatch({ok, {_, #deploy{}}}, R),
            {ok, {Paths, _}} = R,
            ?assertEqual(Result, Paths)
        end}
    end} || {Message, Idents, Deploy, Result} <- [
        {
            "default values selected",
            [{default, [{default, [{default, filename:absname("unknown")}]}]}],
            #deploy{ident=#ident{type=test}},
            [{{default, default, default}, filename:absname("unknown")}]
        },
        {
            "some paths found for ident",
            [{test, [{test, [{branch, filename:absname("__test_test")}]}]}],
            #deploy{ident=#ident{type=test, arch=test}},
            [
                {{test, branch, test}, filename:absname("__test_test")}
            ]
        },
        {
            "some idents in ets, but no path for 'x' ident",
            [
                {test, [{test, [{test, filename:absname("__test_test")}]}]},
                {default, [{default, [{default, filename:absname("unknown")}]}]}
            ],
            #deploy{ident=#ident{type=x, arch=x}},
            [
                {{default, default, default}, filename:absname("unknown")}
            ]
        }
    ]
]}.



copy_packages_test_() ->
{foreachx,
    fun({Paths, #deploy{}}) -> 
        caterpillar_utils:ensure_dir("__test_packages"),
        [caterpillar_utils:ensure_dir(Path) || {_, Path} <- Paths],
        {ok, D} = dets:open_file("__test_dets.deploy", [{access, read_write}]),
        #state{dets = D}
    end,
    fun({Paths, #deploy{packages=Packages}}, #state{dets=D}) ->
        tty_off(),
        dets:close(D),
        file:delete(D),
        [file:close(FD) || #deploy_package{fd=FD} <- Packages],
        caterpillar_utils:del_dir("__test_packages"),
        [caterpillar_utils:del_dir(Path) || {_, Path} <- Paths]
    end,
[
    {{Paths, Deploy}, fun(_, State) ->
        {Message, fun() ->
            Result = (catch caterpillar_deploy:copy_packages({Paths, Deploy}, State)),
            Check(Result, State)
        end}
    end} || {Message, Paths, Deploy, Check} <- [
        {
            "no default path for test, but default/default exists",
            [{{default, default, default}, "__test"}],
            #deploy{packages=[], ident=#ident{arch=test}},
            fun(Result, _) -> ?assertMatch({ok, #deploy{}}, Result) end
        },
        {
            "no default path for test, no default/default exists",
            [],
            #deploy{packages=[], ident=#ident{arch=test}},
            fun(Result, _) -> ?assertEqual({'EXIT', {copy_packages, no_default_path}}, Result) end
            
        },
        {
            "nothing to copy",
            [{{type, default, arch}, "__test"}], 
            #deploy{packages=[], ident=#ident{type=type, arch=arch}},
            fun(Result, _) -> ?assertMatch({ok, #deploy{}}, Result) end
        },
        {
            "some packages copied",
            [
                {{test, test, test}, "__test_test"},
                {{test, default, test}, "__test_default"}
            ],
            #deploy{
                packages=[
                    begin
                        PackageDir = "__test_packages",
                        caterpillar_utils:ensure_dir(PackageDir),
                        AbsPackage = filename:join(PackageDir, Package),
                        file:write_file(AbsPackage, Name),
                        {ok, FD} = file:open(AbsPackage, [read, binary]),
                        #deploy_package{fd=FD, package=Package, name=Name, branch=Branch}
                    end || {Package, Name, Branch} <- [
                        {"package1", "name1", test},
                        {"package2", "name2", test},
                        {"package3", "name3", trunk}
                    ]
                ],
                ident = #ident{type=test, arch=test}
            },
            fun(Result, #state{dets=D}) ->
                ?assertMatch({ok, #deploy{}}, Result),
                ?assertMatch(
                    [
                        {{test, test, "__test_default/package3"}, {"name3", trunk}, _},
                        {{test, test, "__test_test/package1"}, {"name1", test}, _},
                        {{test, test, "__test_test/package2"}, {"name2", test}, _}
                    ],
                    lists:sort(lists:flatten(dets:match(D, '$1')))
                ),
                {ok, Default} = file:list_dir("__test_default"),
                {ok, Test} = file:list_dir("__test_test"),
                ?assertEqual(["package3"], Default),
                ?assertEqual(["package1", "package2"], lists:sort(Test)),
                ?assertEqual(
                    [
                        file:read_file(F) || F <- [
                            "__test_test/package1", "__test_test/package2", "__test_default/package3"
                        ]
                    ],
                    [ {ok,<<"name1">>}, {ok,<<"name2">>}, {ok,<<"name3">>}]
                )
            end
        }
    ]
]}.


run_post_deploy_test_() ->
{foreach,
    fun() -> ok end,
[
    {Message, fun() ->
        register(deploy_test, self()),
        ?assertEqual({ok, done}, catch caterpillar_deploy:run_post_deploy(none, #state{deploy_info=[Deploy]})),
        Check()
    end} || {Message, Deploy, Check} <- [
        {
            "no post_deploy_actions",
            #deploy{},
            fun() -> ok end
        },
        {
            "some postdeploy actions, sending message to self",
            #deploy{post_deploy_actions=[{erlang, send, [deploy_test, post_deploy]}]},
            fun() -> ?assertEqual(post_deploy, caterpillar_test_support:recv(50)) end
        },
        {
            "postdeploy mfa crash",
            #deploy{post_deploy_actions=[{erlang, exit, [normal]}]},
            fun() -> ?assert(true) end
        }
    ]
]}. 


rotate_packages_test_() ->
{foreachx,
    fun(Packages) -> 
        caterpillar_utils:ensure_dir("__test_packages"),
        {ok, D} = dets:open_file("__test_dets.deploy", [{access, read_write}]),
        lists:foreach(
            fun({Type, Arch, Package, Name, Branch, Time}) ->
                filelib:ensure_dir(Package),
                file:write_file(Package, Name),
                dets:insert(D, {{Type, Arch, Package}, {Name, Branch}, Time})
            end,
            Packages
        ),
        #state{dets=D, rotate=2}
    end,
    fun(_, #state{dets=D}) ->
        caterpillar_utils:del_dir("__test_packages"),
        dets:close(D),
        file:delete(D)
    end,
[
    {Packages, fun(_, State) ->
        {Message, fun() ->
            Result = (catch caterpillar_deploy:rotate_packages(Deploy, State)),
            ?assertEqual({ok, done}, Result),
            Check(State)
        end}
    end} || {Message, Packages, Deploy, Check} <- [
        {
            "no package rotated",
            [],
            #deploy{ident=#ident{type=type, arch=arch}},
            fun(_) -> ok end
        },
        {
            "some package rotated, some not",
            [
                {test, test, "__test_packages/p1", "p1", "b1", 1},
                {test, test, "__test_packages/p1_1", "p1", "b1", 2},
                {test, test, "__test_packages/p1_2", "p1", "b1", 3},
                {default, test, "__test_packages/p1_1", "p1", "b1", 2}
            ],
            #deploy{ident=#ident{type=test, arch=test}, packages=[#deploy_package{name="p1", branch="b1"}]},
            fun(#state{dets=D}) ->
                ?assertEqual(
                    [
                        {{default, test, "__test_packages/p1_1"}, {"p1", "b1"}, 2},
                        {{test, test, "__test_packages/p1_1"}, {"p1", "b1"}, 2},
                        {{test, test, "__test_packages/p1_2"}, {"p1","b1"}, 3}
                    ],
                    lists:flatten(lists:sort(dets:match(D, '$1')))
                ),
                {ok, Listing} = file:list_dir("__test_packages"),
                ?assertEqual(["p1_1", "p1_2"], lists:sort(Listing))
            end
        }

    ]
]}.

