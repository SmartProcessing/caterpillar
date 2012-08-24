-module(caterpillar_worker_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

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


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Settings) ->
    {ok, {{simple_one_for_one, 3, 60}, 
            [{
                caterpillar_worker, 
                {caterpillar_worker, start_link, [Settings]},
                permanent,
                5000,
                worker,
                [caterpillar_worker]
            }]}}.

