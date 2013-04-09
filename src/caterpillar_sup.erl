-module(caterpillar_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, application:get_all_env(caterpillar)).


init(Settings) ->
    init_logging(proplists:get_value(logs, Settings, [])),
    NetKernel = init_net_kernel(proplists:get_value(net_kernel, Settings, [])),
    Services = init_services(proplists:get_value(services, Settings, [])),
    Spec = {ok, {{one_for_one, 4, 3600}, [NetKernel|Services]}},
    error_logger:info_msg("initializing childs: ~p~n", Spec),
    Spec.


init_logging(Settings) ->
    error_logger:add_report_handler(caterpillar_rotating_log_handler, Settings).


init_net_kernel(Settings) ->
    {caterpillar_net_kernel, {caterpillar_net_kernel, start_link, [Settings]}, transient, 5000, worker, []}.


init_services(Services) ->
    init_services(Services, []).


init_services([], Accum) ->
    lists:reverse(Accum);
init_services([{Name, Settings}|O], Accum) ->
    Service = {Name, {Name, start_link, [Settings]}, transient, 5000, worker, []},
    init_services(O, [Service|Accum]).
