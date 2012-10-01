-module(caterpillar_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    error_logger:info_msg("starting caterpillar supervisor"),
    case application:get_env(caterpillar, handler) of
        undefined ->
            {error, settings_not_set};
        {ok, Settings} ->
            supervisor:start_link(?MODULE, Settings)
    end.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Settings) ->
    {ok, {{one_for_one, 4, 3600}, 
            [{
                caterpillar, 
                {caterpillar, start_link, [Settings]},
                permanent,
                5000,
                worker,
                [caterpillar]
            }]}}.
