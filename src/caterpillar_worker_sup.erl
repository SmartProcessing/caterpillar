-module(caterpillar_worker_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    case application:get_env(caterpillar, worker) of
        undefined ->
            {error, settings_not_set};
        {ok, Settings} ->
            {ok, Pid} = supervisor:start_link(
                {local, ?MODULE}, ?MODULE, Settings),
            erlang:unlink(Pid),
            {ok, Pid}
    end.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Settings) ->
    {ok, {{simple_one_for_one, 3, 60}, 
            [{
                caterpillar_worker, 
                {caterpillar_worker, start_link, Settings},
                permanent,
                5000,
                worker,
                [caterpillar_worker]
            }]}}.

