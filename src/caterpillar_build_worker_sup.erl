-module(caterpillar_build_worker_sup).

-behaviour(supervisor).

-export([start_link/0, start_link/1]).

-export([init/1]).


-ifndef(TEST).

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

-endif.


-ifdef(TEST).

start_link() ->
    {ok, Pid} = supervisor:start_link(
        {local, ?MODULE}, ?MODULE, []),
    erlang:unlink(Pid),
    {ok, Pid}.

-endif.

start_link(Settings) ->
    {ok, Pid} = supervisor:start_link(
        {local, ?MODULE}, ?MODULE, Settings),
    erlang:unlink(Pid),
    {ok, Pid}.


init(Settings) ->
    {ok, {{simple_one_for_one, 3, 60}, 
            [{
                caterpillar_build_worker, 
                {caterpillar_build_worker, start_link, [Settings]},
                permanent,
                5000,
                worker,
                [caterpillar_build_worker]
            }]}}.