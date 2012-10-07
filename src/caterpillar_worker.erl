-module(caterpillar_worker).

-include_lib("caterpillar.hrl").
-include_lib("caterpillar_worker_internal.hrl").

-behaviour(gen_server).

-export([start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).



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
    IdentList = atom_to_list(Ident),
    State = #state{
        ident = Ident,
        archive_root = caterpillar_utils:ensure_dir(
            ?GV(archive_root, Args, filename:join(?ARCHIVE_ROOT, IdentList))
        ),
        repository_root = caterpillar_utils:ensure_dir(
            ?GV(repository_root, Args, filename:join(?REPOSITORY_ROOT, IdentList))
        )

    },
    case catch init_worker(State, Args) of
        {ok, #state{}} = NewState -> NewState;
        {error, Reason} -> {stop, Reason};
        Error -> error_logger:error_msg("init_worker failed with: ~p~n", [Error]), {stop, crashed}
    end.


handle_info(_Msg, State) -> 
    {noreply, State}.


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
    case Plugin:init_worker(State, WorkerArgs) of
        {ok, WorkerState} -> {ok, State#state{worker_plugin = Plugin, worker_state = WorkerState}};
        Error -> Error
    end.
