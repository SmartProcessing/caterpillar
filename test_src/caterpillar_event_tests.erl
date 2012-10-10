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
        timer:sleep(1),
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
            caterpillar_event:handle_call({register_worker, Service}, {self(), ref}, State),
            ?assertEqual(
                Result,
                lists:sort([{Type, Value} || {_, Type, Value, _} <- ets:tab2list(State#state.ets)])
            )
        end}
    end || {Message, Setup, Service, Result} <- [
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



events_test_() ->
{foreach,
    fun() ->
        caterpillar_event:start_link([])
    end,
    fun(_) ->
        ok = caterpillar_event:stop(),
        timer:sleep(1)
    end,
[
    {Message, fun() ->
        Event(),
        Check()
    end} || {Message, Event, Check} <- [
        {
            "register worker event",
            fun() ->
                caterpillar_event:register_worker(test)
            end,
            fun() ->
                ?assertEqual(
                    [{worker, test}],
                    caterpillar_event:get_info()
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
                caterpillar_event:register_worker(worker1),
                caterpillar_event:register_worker(worker1),
                caterpillar_event:register_worker(worker2),
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
                    caterpillar_event:register_worker(worker1),
                    timer:sleep(5)
                end)
            end,
            fun() ->
                timer:sleep(1),
                ?assertEqual(
                    [{worker, worker1}],
                    caterpillar_event:get_info()
                ),
                timer:sleep(7),
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
                timer:sleep(1),
                ?assertEqual(
                    [{service, service1}],
                    caterpillar_event:get_info()
                ),
                timer:sleep(7),
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
                    receive {_, From, Msg} ->
                        ?assertEqual(
                            {notify, #notify{}},
                            Msg
                        ),
                        gen_server:reply(From, {ok, done})
                    after 50 ->
                        ?assert(false)
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
            "event 'changes', few workers registered",
            fun() ->
                [caterpillar_event:register_worker(W) || W <- [w1, w2]]
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
        }
    ]
]}.
