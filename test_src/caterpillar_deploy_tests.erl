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
    timer:sleep(1),
    ?assertEqual(
        register_as_service,
        receive Msg -> Msg after 50 -> timeout end
    ).


init_ets_test_() ->
{foreachx,
    fun(Data) -> Data end,
    fun(Data, _) ->
        lists:foreach(
            fun({Ident, Branches}) ->
                [caterpillar_utils:del_dir(Branch) || {_, Branch} <- Branches]
            end,
            Data
        )
    end,
[
    {Setup, fun(_, Data) ->
        {Message, fun() ->
            Ets = (catch caterpillar_deploy:init_ets(Data)),
            ?assertEqual(
                Result,
                [
                    {{Ident, Branch}, lists:last(filename:split(Path))} ||
                    {{Ident, Branch}, Path} <- lists:sort(ets:tab2list(Ets))
                ]
            )
        end}
    end} || {Message, Setup, Result} <- [
        {
            "check default deploy in",
            [],
            [{{default, default}, "unknown"}]
        },
        {
            "some idents in cfg, but without default values",
            [{amd64, [{branch, "__test_amd"}]}],
            [
                {{amd64, branch} ,"__test_amd"},
                {{amd64, default}, "unknown"},
                {{default, default}, "unknown"}
            ]
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
        lists:foreach(
            fun({Ident, Branches}) ->
                [caterpillar_utils:del_dir(Branch) || {_, Branch} <- Branches]
            end,
            Idents
        )
    end,
[
    {Idents, fun(_, State) ->
        {Message, fun() ->
            R = caterpillar_deploy:find_deploy_paths(Deploy, State),
            ?assertMatch(
                {ok, {_, #deploy{}}},
                R
            ),
            {ok, {Paths, _}} = R,
            ?assertEqual(
                Result,
                [
                    {{Ident, Branch}, lists:last(filename:split(Path))} ||
                    {{Ident, Branch}, Path} <- Paths
                ]
            )
        end}
    end} || {Message, Idents, Deploy, Result} <- [
        {
            "default values selected",
            [],
            #deploy{ident=test},
            [{{default, default}, "unknown"}]
        },
        {
            "some paths found for ident",
            [{test, [{test, "__test_test"}]}],
            #deploy{ident=test},
            [
                {{test, test}, "__test_test"},
                {{test, default}, "unknown"},
                {{default, default}, "unknown"}
            ]
        },
        {
            "some idents in ets, but no path for 'x' ident",
            [{test, [{test, "__test_test"}]}],
            #deploy{ident=x},
            [
                {{default, default}, "unknown"}
            ]
        }
        
    ]
]}.



copy_packages_test_() ->
{foreachx,
    fun({Paths, #deploy{}}) -> 
        tty_on(),
        caterpillar_utils:ensure_dir("__test_packages"),
        [caterpillar_utils:ensure_dir(Path) || {_, Path} <- Paths],
        {ok, D} = dets:open_file("__test_dets.deploy", [{access, read_write}]),
        #state{dets = D}
    end,
    fun({Paths, #deploy{packages=Packages}}, #state{dets=D}) ->
        dets:close(D),
        file:delete(D),
        [file:close(FD) || #deploy_package{fd=FD} <- Packages],
        caterpillar_utils:del_dir("__test_packages"),
        [caterpillar_utils:del_dir(Path) || {_, Path} <- Paths]
    end,
[
    {{Paths, Deploy}, fun(_, State) ->
        {Message, fun() ->
            ?assertMatch(
                {ok, #deploy{}},
                catch caterpillar_deploy:copy_packages({Paths, Deploy}, State)
            ),
            Check(State)
        end}
    end} || {Message, Paths, Deploy, Check} <- [
        {
            "deploy without packages",
            [{{test, default}, "__test"}], 
            #deploy{packages=[], ident=test},
            fun(_) -> ok end
        },
        {
            "some packages in deploy",
            [
                {{test, test}, "__test_test"},
                {{test, default}, "__test_default"}
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
                ident = test
            },
            fun(#state{dets=D}) ->
                ?assertEqual(
                    [
                        {test, "__test_default/package3", "name3", trunk},
                        {test, "__test_test/package1", "name1", test},
                        {test, "__test_test/package2", "name2", test}
                    ],
                    lists:sort(
                        [
                            {Ident, Package, Name, Branch} || 
                            [{{Ident, Package}, {Name, Branch}, _}] <- dets:match(D, '$1')
                        ]
                    )
                ),
                {ok, Default} = file:list_dir("__test_default"),
                {ok, Test} = file:list_dir("__test_test"),
                ?assertEqual(
                    ["package3"],
                    Default
                ),
                ?assertEqual(
                    ["package1", "package2"],
                    lists:sort(Test)
                )
            end
        }
    ]
]}.
