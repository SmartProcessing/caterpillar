-module(caterpillar_event_tests).

-on_load(tty_off/0).

-include_lib("eunit/include/eunit.hrl").
-include_lib("caterpillar.hrl").
-include_lib("caterpillar_event_internal.hrl").


tty_off() ->
    error_logger:tty(false).


tty_on() ->
    error_logger:tty(true).


start_link_test_() ->
{setup,
    fun() -> ok end,
    fun(_) -> catch caterpillar_event:stop() end,
    fun() ->
        Res = caterpillar_event:start_link([]),
        ?assertMatch({ok, _}, Res),
        {ok, Pid} = Res,
        ?assertEqual(Pid, global:whereis_name(caterpillar_event))
    end
}.


stop_test_() ->
{setup,
    fun() -> caterpillar_event:start_link([]) end,
    fun(_) -> ok end,
    fun() ->
        Pid = global:whereis_name(caterpillar_event),
        ?assert(is_pid(Pid)),
        ?assert(is_process_alive(Pid)),
        caterpillar_event:stop(),
        caterpillar_test_support:wait_for_exit(Pid),
        % timer:sleep(1),
        ?assert(not is_process_alive(Pid))
    end
}.



%%
%% Callbacks tests
%%

handle_call_sync_event_register_service_test_() ->
{foreach,
    fun() -> #state{ets=ets:new(?MODULE, [public])} end,
    fun(#state{ets=Ets}) -> ets:delete(Ets) end,
[
    fun(State) ->
        {Message, fun() ->
            Setup(State),
            caterpillar_event:handle_call({register_service, Service}, {self(), ref}, State),
            ?assertEqual(
                Result,
                lists:sort([{Type, Value} || {_, Type, Value, _} <- ets:tab2list(State#state.ets)])
            )
        end} 
    end || {Message, Setup, Service, Result} <- [
        {
            "ets empty",
            fun(_) -> ok end,
            repository,
            [{service, repository}]
        },
        {
            "ets got some data",
            fun(#state{ets=Ets}) -> ets:insert(Ets, {ref, service, notifier, pid}) end,
            repository,
            [{service, notifier}, {service, repository}]
        },
        {
            "ets already got this service registered",
            fun(#state{ets=Ets}) -> ets:insert(Ets, {ref, service, repository, pid}) end,
            repository,
            [{service, repository}, {service, repository}]
        }
    ]
]}.


handle_call_sync_event_register_worker_test_() ->
{foreach,
    fun() -> #state{ets=ets:new(?MODULE, [public])} end,
    fun(#state{ets=Ets}) -> ets:delete(Ets) end,
[
    fun(State) ->
        {Message, fun() ->
            Setup(State),
            caterpillar_event:handle_call({register_worker, Worker, work_id}, {self(), ref}, State),
            ?assertEqual(
                Result,
                lists:sort([{Type, Value} || {_, Type, Value, _} <- ets:tab2list(State#state.ets)])
            )
        end}
    end || {Message, Setup, Worker, Result} <- [
        {
            "ets empty",
            fun(_) -> ok end,
            amd64,
            [{worker, amd64}]
        },
        {
            "ets got some data",
            fun(#state{ets=Ets}) -> ets:insert(Ets, {ref, service, notifier, pid}) end,
            amd64,
            [{service, notifier}, {worker, amd64}]
        },
        {
            "ets already got this service registered",
            fun(#state{ets=Ets}) -> ets:insert(Ets, {ref, worker, amd64, pid}) end,
            amd64,
            [{worker, amd64}, {worker, amd64}]
        }
        
    ]
]}.


select_service_test_() ->
{foreachx,
    fun(Services) ->
        Ets = ets:new(?MODULE, [protected]),
        ets:insert(Ets, Services),
        Ets
    end,
    fun(_, Ets) ->
        ets:delete(Ets)
    end,
[
    {Services, fun(_, Ets) ->
        {Message, fun() ->
            ?assertEqual(
                Result,
                caterpillar_event:select_service(Ets, ServiceName)
            )
        end}
    end} || {Message, Services, ServiceName, Result} <- [
        {
            "no services",
            [], 
            some_service,
            {error, no_service}
        },
        {
            "some service available",
            [{ref1, service, some_service, pid}],
            some_service,
            {ok, pid}
        },
        {
            "some services and workers available",
            [
                {ref1, service, some_service, pid1}, {ref2, service, some_service, pid2},
                {ref3, worker, some_worker, pid2}
            ],
            some_service,
            {ok, pid2}
        }
    ]
]}.


select_worker_test_() ->
{foreachx,
    fun(Services) ->
        Ets = ets:new(?MODULE, [protected]),
        ets:insert(Ets, Services),
        Ets
    end,
    fun(_, Ets) ->
        ets:delete(Ets)
    end,
[
    {Services, fun(_, Ets) ->
        {Message, fun() ->
            ?assertEqual(
                Result,
                caterpillar_event:select_worker(Ets, ServiceName)
            )
        end}
    end} || {Message, Services, ServiceName, Result} <- [
        {
            "no workers",
            [], 
            some_worker,
            {error, no_worker}
        },
        {
            "some worker available",
            [{ref1, worker, some_worker, pid}],
            some_worker,
            {ok, pid}
        },
        {
            "some workers and workers available",
            [
                {ref1, worker, some_worker, pid1}, {ref2, worker, some_worker, pid2},
                {ref3, worker, some_worker, pid2}
            ],
            some_worker,
            {ok, pid2}
        }
    ]
]}.


select_worker_pids_test_() ->
{foreach,
    fun() -> 
        Ets = ets:new(t, [public])
    end,
[
    fun(Ets) ->
        {Message, fun() ->
            lists:foreach(
                fun(W) -> ets:insert(Ets, {erlang:make_ref(), worker, W, pid}) end,
                Workers
            ),
            ?assertEqual(
                Result,
                caterpillar_event:select_workers_pids(Ets)
            )
        end}
    end || {Message, Workers, Result} <- [
        {
            "no workers",
            [],
            []
        },
        {
            "some workers",
            [w1, w2],
            [pid, pid]
        }
    ]
]}.



events_test_() ->
{foreach,
    fun() ->
        caterpillar_event:start_link([])
    end,
    fun(_) ->
        Pid = global:whereis_name(caterpillar_event),
        ok = caterpillar_event:stop(),
        caterpillar_test_support:wait_for_exit(Pid)
        % timer:sleep(1)
    end,
[
    {Message, fun() ->
        Event(),
        Check()
    end} || {Message, Event, Check} <- [
        {
            "register worker event",
            fun() ->
                caterpillar_event:register_worker(test, work_id)
            end,
            fun() ->
                ?assertEqual(
                    [{worker, test}],
                    caterpillar_event:get_info()
                )
            end
        },
        {
            "register worker event, checking repository service got event about new worker",
            fun() ->
                caterpillar_event:register_service(repository),
                caterpillar_event:register_worker(test, work_id)
            end,
            fun() ->
                ?assertEqual(
                    [{service, repository}, {worker, test}],
                    lists:sort(caterpillar_event:get_info())
                ),
                timer:sleep(10),
                ?assertMatch(
                    {messages, [{_, _, {get_archives, work_id}}]},
                    process_info(self(), messages)
                )
            end
        },
        {
            "register service event",
            fun() ->
                caterpillar_event:register_service(test)
            end,
            fun() ->
                ?assertEqual(
                    [{service, test}],
                    caterpillar_event:get_info()
                )
            end
        },
        {
            "register few workers and few services",
            fun() ->
                caterpillar_event:register_worker(worker1, work_id),
                caterpillar_event:register_worker(worker1, work_id),
                caterpillar_event:register_worker(worker2, work_id),
                caterpillar_event:register_service(test1),
                caterpillar_event:register_service(test1),
                caterpillar_event:register_service(test2)
            end,
            fun() ->
                ?assertEqual(
                    [
                        {service,test1},
                        {service,test1},
                        {service,test2},
                        {worker,worker1},
                        {worker,worker1},
                        {worker,worker2}
                    ],
                    lists:sort(caterpillar_event:get_info())
                )
            end
        },
        {
            "registered worker down",
            fun() ->
                spawn(fun() ->
                    caterpillar_event:register_worker(worker1, work_id),
                    timer:sleep(10)
                end)
            end,
            fun() ->
                timer:sleep(5),
                ?assertEqual(
                    [{worker, worker1}],
                    caterpillar_event:get_info()
                ),
                timer:sleep(10),
                ?assertEqual(
                    [],
                    caterpillar_event:get_info()
                )
            end
        },
        {
            "registered service down",
            fun() ->
                spawn(fun() ->
                    caterpillar_event:register_service(service1),
                    timer:sleep(5)
                end)
            end,
            fun() ->
                timer:sleep(2),
                ?assertEqual(
                    [{service, service1}],
                    caterpillar_event:get_info()
                ),
                timer:sleep(8),
                ?assertEqual(
                    [],
                    caterpillar_event:get_info()
                )
            end
        },
        {
            "sync event, notify, not notifier available",
            fun() -> ok end,
            fun() ->
                ?assertEqual(
                    {error, no_service},
                    caterpillar_event:sync_event({notify, #notify{}})
                )
            end
        },
        {
            "sync event, notify, notifier available",
            fun() -> 
                spawn(fun() ->
                    caterpillar_event:register_service(notifier),
                    receive {_, From, {notify, #notify{}}} ->
                        gen_server:reply(From, {ok, done})
                    after 10 ->
                        timeout
                    end
                end)
            end,
            fun() ->
                ?assertEqual(
                    {ok, done},
                    caterpillar_event:sync_event({notify, #notify{}})
                )
            end
        },
        {
            "sync event, get_archive, no repository available",
            fun() -> ok end,
            fun() ->
                ?assertEqual(
                    {error, no_service},
                    caterpillar_event:sync_event({get_archive, #archive{}})
                )
            end
        },
        {
            "sync event, get_archive, repository available",
            fun() -> 
                spawn(fun() ->
                    caterpillar_event:register_service(repository),
                    receive {_, From, {get_archive, #archive{}}} ->
                        gen_server:reply(From, ok)
                    after 10 ->
                        timeout
                    end
                end)
            end,
            fun() ->
                timer:sleep(1),
                ?assertEqual(
                    [{service, repository}],
                    caterpillar_event:get_info()
                ),
                ?assertEqual(
                    ok,
                    caterpillar_event:sync_event({get_archive, #archive{}})
                )
            end
        },
        {
            "sync event rescan_repository",
            fun() -> 
                spawn(fun() ->
                    caterpillar_event:register_service(repository),
                    receive {_, From, rescan_repository} ->
                        gen_server:reply(From, ok)
                    after 10 ->
                        timeout
                    end
                end)
            end,
            fun() ->
                timer:sleep(1),
                ?assertEqual(
                    [{service, repository}],
                    caterpillar_event:get_info()
                ),
                ?assertEqual(
                    ok,
                    caterpillar_event:sync_event(rescan_repository)
                )
            end
        }, 
        {
            "sync event rebuild_package",
            fun() -> 
                spawn(fun() ->
                    caterpillar_event:register_service(repository),
                    receive {_, From, {rebuild_package, {package, branch}}} ->
                        gen_server:reply(From, ok)
                    after 10 ->
                        timeout
                    end
                end)
            end,
            fun() ->
                timer:sleep(1),
                ?assertEqual(
                    [{service, repository}],
                    caterpillar_event:get_info()
                ),
                ?assertEqual(
                    ok,
                    caterpillar_event:sync_event({rebuild_package, {package, branch}})
                )
            end
        }, 
        {
            "sync event repository custom command",
                        fun() -> 
                spawn(fun() ->
                    caterpillar_event:register_service(repository),
                    receive {_, From, {repository_custom_command, command, args}} ->
                        gen_server:reply(From, ok)
                    after 10 ->
                        timeout
                    end
                end)
            end,
            fun() ->
                timer:sleep(1),
                ?assertEqual(
                    [{service, repository}],
                    caterpillar_event:get_info()
                ),
                ?assertEqual(
                    ok,
                    caterpillar_event:sync_event({repository_custom_command, command, args})
                )
            end
        },
        {
            "event 'changes', few workers registered",
            fun() ->
                [caterpillar_event:register_worker(W, work_id) || W <- [w1, w2]]
            end,
            fun() ->
                ?assertEqual(
                    [{worker, w1}, {worker, w2}],
                    lists:sort(caterpillar_event:get_info())
                ),
                caterpillar_event:event({changes, work_id, [#archive{}]}),
                Receive = fun() ->
                    receive {_, Msg} ->
                        Msg
                    after 10 ->
                        timeout
                    end
                end,
                ?assertEqual(
                    [{changes, work_id, [#archive{}]} || _ <- [w1, w2]],
                    [Receive() || _ <- [w1, w2]]
                )
            end
        },
        {
            "event 'clean_packages', few workers registered",
            fun() ->
                [caterpillar_event:register_worker(W, work_id) || W <- [w1, w2]]
            end,
            fun() ->
                ?assertEqual(
                    [{worker, w1}, {worker, w2}],
                    lists:sort(caterpillar_event:get_info())
                ),
                caterpillar_event:event({clean_packages, [#archive{}]}),
                Receive = fun() ->
                    receive {_, Msg} ->
                        Msg
                    after 10 ->
                        timeout
                    end
                end,
                ?assertEqual(
                    [{clean_packages, [#archive{}]} || _ <- [w1, w2]],
                    [Receive() || _ <- [w1, w2]]
                )
            end
        }
    ]
]}.



sync_event_to_service_test_() ->
{foreachx,
    fun(Setup) ->
        Ets = ets:new(?MODULE, [public]),
        ets:insert(Ets, Setup),
        Ets
    end,
    fun(_, Ets) ->
        ets:delete(Ets)
    end,
[
    {Setup, fun(_, Ets) ->
        {Message, fun() ->
            From = {self(), ref},
            Mock(),
            caterpillar_event:sync_event_to_service(test, From, Ets, request), 
            Check()
        end}
    end} || {Message, Setup, Mock, Check} <- [
        {
            "no service in ets",
            [],
            fun() -> ok end,
            fun() -> 
                ?assertEqual(
                    {error, no_service},
                    receive {_, Msg} -> Msg after 50 -> timeout end
                )
            end
        },
        {
            "service in ets, but crashes after query",
            [{ref, service, test, test_service}],
            fun() ->
                spawn(fun() ->
                    register(test_service, self()),
                    receive _ ->
                        exit(normal)
                    after 50 ->
                        ok
                    end
                end),
                timer:sleep(5)
            end,
            fun() ->
                ?assertEqual(
                    {'EXIT', {normal,{gen_server,call,[test_service,request,infinity]}}},
                    receive {_, Msg} -> Msg after 50 -> timeout end
                )
            end
        },
        {
            "service in ets, returns response",
            [{ref, service, test, test_service}],
            fun() ->
                spawn(fun() ->
                    register(test_service, self()),
                    receive {_, From, _} ->
                        gen_server:reply(From, ok)
                    after 50 ->
                        ok
                    end
                end),
                timer:sleep(5)
            end,
            fun() ->
                ?assertEqual(
                    ok, 
                    receive {_, Msg} -> Msg after 50 -> timeout end
                )
            end
        }
        
    ]
]}.
