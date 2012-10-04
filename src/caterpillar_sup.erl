-module(caterpillar_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).


start_link() ->
    error_logger:info_msg("starting caterpillar supervisor~n"),
    case application:get_env(caterpillar, handler) of
        undefined ->
            {error, settings_not_set};
        {ok, Settings} ->
            supervisor:start_link(?MODULE, Settings)
    end.


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
