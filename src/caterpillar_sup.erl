-module(caterpillar_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, application:get_all_env(caterpillar)).


init(Settings) ->
    NetKernel = init_net_kernel(proplists:get_value(net_kernel, Settings, [])),
    Services = init_services(proplists:get_value(services, Settings, [])),
    {ok, {{one_for_one, 4, 3600}, Services ++ [NetKernel]}}.


init_net_kernel(Settings) ->
    {caterpillar_net_kernel, {caterpillar_net_kernel, start_link, [Settings]}, transient, 5000, worker, []}.


init_services(Services) ->
    init_services(Services, []).


init_services([], Accum) ->
    lists:reverse(Accum);
init_services([{Name, Settings}|O], Accum) ->
    Service = {Name, {Name, start_link, [Settings]}, transient, 5000, worker, []},
    init_services(O, [Service|Accum]).
