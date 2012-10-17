-module(caterpillar_worker).

-include_lib("caterpillar.hrl").
-include_lib("caterpillar_worker_internal.hrl").

-behaviour(gen_server).

-export([start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([retrieve_archive/1, deploy/0]).



start_link(Args) ->
    case proplists:get_value(ident, Args, '$none') of
        '$none' ->
            error_logger:error_msg("no ident given, stopping~n"),
            {error, no_ident};
        Ident when is_atom(Ident) ->
            gen_server:start_link({local, Ident}, ?MODULE, Args, []);
        Ident ->
            error_logger:error_msg("bad ident type: ~p~n", [Ident]),
            {error, bad_ident}
    end.


stop(Ident) ->
    gen_server:call(Ident, stop, infinity).




init(Args) ->
    Ident = ?GV(ident, Args),
    State = #state{
        worker_pid = self(),
        ident = Ident

    },
    case catch init_worker(State, Args) of
        {ok, #state{}} = NewState -> NewState;
        {error, Reason} -> {stop, Reason};
        Error -> error_logger:error_msg("init_worker failed with: ~p~n", [Error]), {stop, crashed}
    end.


handle_info(_Msg, State) -> 
    {noreply, State}.


handle_cast({changes, WorkId, Archives}, #state{worker_plugin=WP, worker_state=WS}=State) ->
    NewWorkerState = WP:changes(WS, WorkId, Archives),
    {noreply, State#state{worker_state=NewWorkerState}};
handle_cast({deploy, WorkId, Deploy}, #state{worker_plugin=WP, worker_state=WS}=State) ->
    NewWorkerState = WP:deploy(WorkId, Deploy),
    {noreply, State#state{worker_state=NewWorkerState}};
handle_cast(_Msg, State) ->
    {noreply, State}.


handle_call(_Msg, _From, State) ->
    {reply, {error, bad_msg}, State}.


terminate(Reason, #state{ident=Ident}) ->
    error_logger:info_msg("worker(~p) down with reason: ~p~n", [Ident, Reason]).


code_change(_Old, State, _Extra) ->
    {ok, State}.



%------


init_worker(State, Args) ->
    Plugin = proplists:get_value(worker_plugin, Args),
    WorkerArgs = proplists:get_value(worker_plugin_init, Args),
    case Plugin:init_worker(WorkerArgs) of
        {ok, WorkerState} -> {ok, State#state{worker_plugin = Plugin, worker_state = WorkerState}};
        Error -> Error
    end.



%------


retrieve_archive(#archive{}=A) -> 
    caterpillar_event:sync_event({get_archive, A}).


deploy() -> ok.
