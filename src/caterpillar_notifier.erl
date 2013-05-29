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
    EmailTo = proplists:get_value(email_to, Args),
    State = #state{
        ets = ets:new(?MODULE, [protected, named_table]),
        mail_root=caterpillar_utils:ensure_dir(proplists:get_value(mail_root, Args, ?MAIL_ROOT)),
        email_from=proplists:get_value(email_from, Args),
        email_to = EmailTo,
        email_distribution=proplists:get_value(email_distribution, Args, [EmailTo])
    },
    async_send_mail(),
    async_register(),
    {ok, State}.


handle_info({_Port, {data, Data}}, State) ->
    error_logger:error_msg("Data from port ~s~n", [Data]),
    {noreply, State};

handle_info({Port, {exit_status, Status}}, #state{ets=Ets, mail_root=MR}=State) ->
    case {Status, ets:lookup(Ets, Port)} of
        {0, [{Port, File}]} -> file:delete(filename:join(MR, File));
        {1, [{Port, File}]}-> error_logger:info_msg("notifier failed to send ~s~n", [File]);
        _ -> ok
    end,
    async_send_mail(0),
    ets:delete(Ets, Port),
    {noreply, State};

handle_info({'DOWN', _, _, _, _}, State) ->
    async_register(),
    {noreply, State#state{registered=false}};

handle_info(async_send_mail, #state{mail_root=MR, ets=Ets}=State) ->
    case file:list_dir(MR) of
        {ok, [File|_]} -> ets:insert(Ets, {send_mail(State, File), File});
        _ -> async_send_mail(1000)
    end,
    {noreply, State};

handle_info(async_register, #state{registered=false}=State) ->
    case catch caterpillar_event:register_service(notifier) of
        {ok, Pid} -> 
            erlang:monitor(process, Pid),
            {noreply, State#state{registered=true}};
        _ ->
            async_register(),
            {noreply, State}
    end;

handle_info(_Message, State) ->
    {noreply, State}.


handle_cast(_Messages, State) ->
    {noreply, State}.


handle_call({notify, Notify}, From, State) ->
    spawn(fun() ->
        gen_server:reply(From, catch store_mail(State, Notify))
    end),
    {noreply, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Message, _From, State) ->
    {reply, {error, bad_msg}, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%--------


get_name() -> lists:flatten(io_lib:format("~4..0B~6..0B~6..0B", tuple_to_list(erlang:now()))).


async_register() -> 
    async_register(1000).


async_register(Delay) ->
    erlang:send_after(Delay, self(), async_register).


async_send_mail() -> async_send_mail(1000).

async_send_mail(Delay) ->
    erlang:send_after(Delay, self(), async_send_mail).


send_mail(#state{mail_root=MR, email_distribution=DistList}, File) ->
    AbsPath = filename:join(MR, File),
    Emails = string:join(DistList, " "),
    Cmd = lists:flatten(io_lib:format("ssmtp ~s < ~s", [Emails, AbsPath])),
    open_port({spawn, Cmd}, [binary, stderr_to_stdout, exit_status]).


store_mail(#state{mail_root=MR, email_from=EFrom, email_to=ETo}, #notify{subject=Sub, body=Body}) ->
    file:write_file(filename:join(MR, get_name()), io_lib:format(?MAIL_TEMPLATE, [ETo, EFrom, Sub, Body])).




