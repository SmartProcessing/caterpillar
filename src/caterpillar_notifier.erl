-module(caterpillar_notifier).

-behaviour(gen_server).
-include_lib("caterpillar.hrl").
-include_lib("caterpillar_notifier_internal.hrl").


-export([start_link/1, stop/0]).
-export([init/1, handle_info/2, handle_cast/2, handle_call/3, terminate/2, code_change/3]).




start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).


stop() ->
    ok = gen_server:call(?MODULE, stop).


init(Args) ->
    State = #state{
        mail_root=caterpillar_utils:ensure_dir(proplists:get_value(mail_root, Args, ?MAIL_ROOT)),
        email_to=proplists:get_value(email_to, Args),
        email_from=proplists:get_value(email_from, Args)
    },
    %FIXME:
    get_name(),
    {ok, State}.


handle_info({_Port, {data, Data}}, State) ->
    error_logger:error_msg("Data from port ~ts~n", [Data]),
    {noreply, State};
handle_info({_Port, {exit_status, Status}}, State) ->
    case Status of
        Status -> ok
    end,
    {noreply, State};
handle_info(_Message, State) ->
    {noreply, State}.


handle_cast(_Messages, State) ->
    {noreply, State}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Message, _From, State) ->
    {reply, ok, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


get_name() ->
    lists:flatten(
        io_lib:format("~4..0B~6..0B~6..0B", tuple_to_list(os:timestamp()))
    ).
