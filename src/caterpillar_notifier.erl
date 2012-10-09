-module(caterpillar_notifier).

-behaviour(gen_server).

-export([start_link/1, stop/0]).

-export([init/1, handle_info/2, handle_cast/2, handle_call/3, terminate/2, code_change/3]).

-record(state, {
    mail_dir,
    email_to,
    email_from
}).


-define(MAIL_TEMPLATE,
    "To: ~s~n" ++
    "From: ~s~n" ++
    "Subject: ~s~n" ++
    "~n~n" ++
    "~ts~n"
).

-define(FL, lists:flatten).
-define(MAX_SIZE, 10*1024).


start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).


stop() ->
    ok = gen_server:call(?MODULE, stop).


init(Args) ->
    NotifierDir = filename:absname(proplists:get_value(dir, Args)),
    buildnet_utils:make_dir(NotifierDir),
    EmailTo = proplists:get_value(email_to, Args),
    EmailFrom = proplists:get_value(email_from, Args),
    MailDir = filename:absname(filename:join(NotifierDir, "mail")),
    buildnet_utils:make_dir(MailDir),

    State = #state{
        mail_dir=MailDir,
        email_to=EmailTo,
        email_from=EmailFrom
    },
    self() ! check_mail,
    {ok, State}.


handle_info({_Port, {data, Data}}, State) ->
    error_logger:error_msg("Data from port ~ts~n", [Data]),
    {noreply, State};
handle_info({_Port, {exit_status, Status}}, State) ->
    Mail = get_mail(State),
    MailName = lists:last(filename:split(Mail)),
    case Status of
        0 ->
            error_logger:info_msg("Mail ~p delivered~n", [MailName]),
            delete_mail(State, Mail);
        _ ->
            error_logger:error_msg("Mail ~p failed to deliver~n", [MailName]),
            timer:sleep(10000)
    end,
    self() ! check_mail,
    {noreply, State};
handle_info(check_mail, State) ->
    case get_mail(State) of
        none ->
            erlang:send_after(10000, self(), check_mail);
        File ->
            send_mail(State, File)
    end,
    {noreply, State};
handle_info(_Message, State) ->
    {noreply, State}.


handle_cast({notify, Data}, State) ->
    store_mail(State, Data),
    erlang:garbage_collect(), %dont need garbage here
    {noreply, State};
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
    {M, S, Ms} = os:timestamp(),
    "mail_" ++ ?FL([ integer_to_list(X) || X <- [M, S, Ms] ]).


store_mail(State, Data) ->
    EmailFrom = State#state.email_from,
    EmailTo = State#state.email_to,
    case store_mail(Data) of
        {Subject, Body} ->
            MailDir = State#state.mail_dir,
            Name = get_name(),
            AbsPath = filename:join(MailDir, Name),
            error_logger:info_msg("storing mail at file ~p~n", [Name]),
            Template = prep_template(
                io_lib:format(
                    ?MAIL_TEMPLATE,
                    [EmailTo, EmailFrom, ?FL(Subject), ?FL(Body)]
                )
            ),
            file:write_file(AbsPath, Template);
        _ ->
            ok
    end.


prep_template(List) ->
    NewList = lists:flatten(List),
    Max = lists:max(NewList),
    if 
        Max > 255 ->
            unicode:characters_to_binary(NewList);
        true ->
            list_to_binary(NewList)
    end.


store_mail({deploy, Bid, NotifyData}) ->
    Subject = io_lib:format("deploy for build #~p", [Bid]),
    Body = format_data(deploy, NotifyData),
    {Subject, Body};
store_mail({branches_deleted, Branches})->
    Subject = "some branches deleted",
    Body = format_data(branches_deleted, Branches),
    {Subject, Body};
store_mail({changes, BuildId, Data}) ->
    Subject = io_lib:format("changes for build #~p", [BuildId]),
    Body = format_data(changes, Data),
    {Subject, Body};
store_mail({not_packed, BuildId, NotPacked}) ->
    Subject = io_lib:format("pack error for build #~p", [BuildId]),
    Body = format_data(not_packed, NotPacked),
    {Subject, Body};
store_mail({not_unfolded, BuildId, NotUnfolded}) ->
    Subject = io_lib:format("unfold error for build #~p", [BuildId]),
    Body = format_data(not_unfolded, NotUnfolded),
    {Subject, Body};
store_mail(Data) ->
    error_logger:error_msg("unknown data format ~p~n", [Data]).


format_data(Type, Data) ->
    format_data(Type, Data, []).


format_data(_Type, [], Accum) ->
    lists:sort(Accum);

format_data(changes, [ {Branch, Changes, Diff}|T ], Accum) ->
    LC = length(Changes),
    LD = length(Diff),
    NChanges = if
        LC > ?MAX_SIZE ->
            lists:sublist(Changes, ?MAX_SIZE) ++ "...\n";
        true ->
            Changes
    end,
    NDiff = if 
        LD > ?MAX_SIZE ->
            lists:sublist(Diff, ?MAX_SIZE) ++ "...\n";
        true ->
            Diff
    end,
    Data = ?FL(
        [branch2str(Branch), "\n", NChanges, "\n\n", NDiff, "\n\n"]
    ),
    erlang:garbage_collect(),
    format_data(changes, T, [ Data|Accum ]);

format_data(branches_deleted, [ {Branch, Revno}|T ], Accum) ->
    Data = io_lib:format("~ts with revno ~p deleted\n", [branch2str(Branch), Revno]),
    format_data(branches_deleted, T, [ Data|Accum ]);

format_data(not_packed, [ {Branch, _, Revno}|T ], Accum) ->
    Data = io_lib:format("cant pack ~p:~p~n", [branch2str(Branch), Revno]),
    format_data(not_packed, T, [ Data|Accum ]);

format_data(not_unfolded, [ {Branch, {PrevRev, CurRev}}|T ], Accum) ->
    Data = io_lib:format(
        "cant unfold ~p:~p-~p~n",
        [branch2str(Branch), PrevRev, CurRev]
    ),
    format_data(not_unfolded, T, [ Data|Accum ]);

format_data(deploy, [ H|T ], Accum) ->
    Data = case H of
        {ok, Name} ->
            io_lib:format("ok ~ts\n", [branch2str(Name)]);
        {error, Name, Log} ->
            io_lib:format("error ~ts~n~ts~n~n", [branch2str(Name), Log]);
        Other ->
            error_logger:error_msg(
                "format data deploy got strange struct ~p~n",
                [Other]
            )
        end,
        format_data(deploy, T, [ Data|Accum]);

format_data(Other, _, _) ->
    error_logger:error_msg("notifier dont know how format ~p~n", [Other]),
    [].


get_mail(State) ->
    MailDir = State#state.mail_dir,
    List = filelib:wildcard(filename:join([MailDir,  "mail_*"])),
    SortedList = lists:sort(List),
    case SortedList of
        [] ->
            none;
        [ First|_ ] ->
            First
    end.
        

delete_mail(State, Mail) ->
    AbsPath = get_abs_path(State, Mail),
    file:delete(AbsPath).


send_mail(State, Mail) ->
    AbsPath = get_abs_path(State, Mail),
    Cmd = ?FL(
        io_lib:format(
            "ssmtp ~ts < ~ts", [State#state.email_to, AbsPath]
        )
    ),
    open_port({spawn, Cmd}, [binary, stderr_to_stdout, exit_status]).


get_abs_path(State, Mail) ->
    filename:join(State#state.mail_dir, Mail).


branch2str({Repo, Branch}) ->
    string:join([Repo, Branch], "/").

