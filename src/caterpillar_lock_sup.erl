-module(caterpillar_lock_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).


start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 3, 60}, 
            [{
                caterpillar_lock, 
                {caterpillar_lock, start_link, []},
                transient,
                5000,
                worker,
                [caterpillar_lock]
            }]}}.
