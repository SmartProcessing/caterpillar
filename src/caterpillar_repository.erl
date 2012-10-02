-module(caterpillar_repository).

-behaviour(gen_server).

-include_lib("caterpillar_repository_internal.hrl").

-export([start_link/1, stop/0]).
-export([init/1, handle_info/2, handle_cast/2, handle_call/3, terminate/2, code_change/3]).



start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).


stop() -> 
    gen_server:call(?MODULE, stop, infinity).


init(Args) ->
    State = vcs_init(
        #state{
            repository_root = filename:absname(proplists:get_value(repository_root, Args, ".")),
            archive_root = filename:absname(proplists:get_value(archive_root, Args, ?ARCHIVE_PATH)),
            scan_interval = proplists:get_value(scan_interval, Args, ?SCAN_INTERVAL)
        },
        Args
    ),
    {ok, scan_repository(State)}.



handle_info(_Msg, State) ->
    {noreply, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Msg, _From, State) ->
    {reply, {error, bad_msg}, State}.


terminate(Reason, _State) ->
    error_logger:info_msg("caterpillar_repository down with reason ~p~n", [Reason]).


code_change(_Old, State, _Extra) ->
    {ok, State}.



%-------


-spec vcs_init(State::#state{}, Args::proplists:property()) -> NewState::#state{}.

vcs_init(State, Args) ->
    case catch vcs_init_(State, Args) of
        #state{}=NewState -> NewState;
        Error ->
            error_logger:error_msg("vcs_init failed with: ~p~n", [Error]),
            exit({vcs_init, failed})
    end.


vcs_init_(State, Args) ->
    VcsPlugin = proplists:get_value(vcs_plugin, Args),
    case VcsPlugin:init(proplists:get_value(vcs_plugin_init, Args, [])) of
        {ok, VcsState} ->
            State#state{vcs_plugin=VcsPlugin, vcs_state=VcsState};
        Error -> Error
    end.


-spec scan_repository(#state{}|non_neg_integer()) -> #state{}|reference().

scan_repository(#state{scan_interval=SI, scan_timer=ST}=State) ->
    catch erlang:cancel_timer(ST),
    State#state{scan_timer=scan_repository(SI)};
scan_repository(Delay) when is_integer(Delay), Delay >= 0 ->
    erlang:send_after(Delay, self(), scan_repository).
