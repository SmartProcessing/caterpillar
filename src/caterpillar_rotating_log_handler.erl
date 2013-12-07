-module(caterpillar_rotating_log_handler).

-behaviour(gen_event).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-define(WM, fun write_msg/3).
-define(WR, fun write_report/3).



-record(state, {
    log_file,
    log_fd,
    log_size,
    log_max_size,
    log_backup_count,
    report_file,
    report_fd,
    report_size,
    report_max_size,
    report_backup_count,
    log_level=all
}).


init(Args) ->
    State = init_report(init_log(#state{}, Args), Args),
    {ok, State}.


init_log(State, Args) ->
    LogLevel = proplists:get_value(log_level, Args, all),
    LogFile = proplists:get_value(log_file, Args),
    filelib:ensure_dir(LogFile),
    MaxSize = proplists:get_value(log_max_size, Args, 5242880),
    BackupCount = proplists:get_value(log_backup_count, Args, 5),
    Size = filelib:file_size(LogFile),
    NewState = State#state{
        log_file=LogFile,
        log_size=Size,
        log_max_size=MaxSize,
        log_backup_count=BackupCount,
        log_level=log_level(LogLevel)
    },
    rotate_if_needed(log, NewState, 0).


init_report(St, Args) -> 
    ReportFile = proplists:get_value(report_file, Args, St#state.log_file),
    filelib:ensure_dir(ReportFile),
    MaxSize = proplists:get_value(report_max_size, Args, St#state.log_max_size),
    BackupCount = proplists:get_value(
        report_backup_count, Args, St#state.log_backup_count
    ),
    Size = filelib:file_size(ReportFile),
    NewState = St#state{
        report_file=ReportFile,
        report_size=Size,
        report_max_size=MaxSize,
        report_backup_count=BackupCount
    },
    rotate_if_needed(report, NewState, 0).



handle_event({error, _Gl, Info}, State) ->
    log_checker(error, State, ?WM, [State, "ERROR", Info]);

handle_event({error_report, _Gl, Info}, State) ->
    log_checker(error_report, State, ?WR, [State, "ERROR REPORT", Info]);

handle_event({warning_msg, _Gl, Info}, State) ->
    log_checker(warning_msg, State, ?WM, [State, "WARNING", Info]);

handle_event({warning_report, _Gl, Info}, State) ->
    log_checker(warning_report, State, ?WR, [State, "WARNING ERROR", Info]);

handle_event({info_msg, _Gl, Info}, State) ->
    log_checker(info_msg, State, ?WM, [State, "INFO", Info]);

handle_event({info_report, _Gl, Info}, State) ->
    log_checker(info_report, State, ?WR, [State, "INFO REPORT", Info]);

handle_event(_Event, State) ->
    {ok, State}.


handle_call(_Request, State) ->
    {ok, {error, badarg}, State}.


handle_info(_Info, State) ->
    {ok, State}.


terminate(_Arg, State) ->
    {Report, Log} = {State#state.report_fd, State#state.log_fd},
    file:sync(Report),
    file:sync(Log),
    ok = file:close(State#state.report_fd),
    ok = file:close(State#state.log_fd).


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%
%%% Auxiliary functions
%%%

get_time() ->
    {{Y, M, D}, {H, Mi, S}} = erlang:localtime(),
    {_, _, Ms} = erlang:now(),
    get_time(Y, M, D, H, Mi, S, Ms).


get_time(Y, M, D, H, Mi, S, Ms) ->
    io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B:~5..0B",
        [Y, M, D, H, Mi, S, normalize(Ms)]).


normalize(0) ->
    0;
normalize(Ms) when Ms >= 100000 ->
    normalize(round(Ms/10));
normalize(Ms) when Ms < 10000 ->
    normalize(Ms * 10);
normalize(Ms) ->
    Ms.


write_msg(State, Type, {Pid, Format, Data}) ->
    NewFormat = format_message(
        "~s ~s ~p ~s",
        [get_time(), Type, Pid, Format]
    ),
    Msg = format_message(NewFormat, Data),
    write(msg, State, Msg).


write_report(State, Type, {Pid, MsgType, Report}) ->
    Msg = format_message(
        "~s ~s ~p [~p] ~p~n",
        [get_time(), Type, Pid, MsgType, Report]
    ),
    write(report, State, Msg).


%write_unknown(State, Event) ->
%    Msg = format_message("~s UNKNOWN ~p~n", [get_time(), Event]),
%    write(unknown, State, Msg).


write(_, State, <<>>) ->
    {ok, State};
write(Type, State, Msg) ->
    NewState = rotate_if_needed(Type, State, length(Msg)),
    write_data(Type, NewState, Msg).


format_message(Template, Params) ->
    try
        lists:flatten(io_lib:format(Template, Params))
    catch _:_ ->
        <<"">>
    end.



write_data(report, State, Msg) ->
    Fd = State#state.report_fd,
    write_data(Fd, Msg),
    {ok, State};
write_data(_Other, State, Msg) ->
    Fd = State#state.log_fd,
    write_data(Fd, Msg),
    {ok, State}.
    

write_data(Fd, Msg) ->
    file:write(Fd, Msg).


rotate_if_needed(report, State, Size) ->
    FileName = State#state.report_file,
    FileFd = State#state.report_fd,
    CurSize = State#state.report_size,
    MaxSize = State#state.report_max_size,
    BackupCount = State#state.report_backup_count,
    {NewFd, NewSize} = rotate_if_needed(
        FileName, FileFd, Size, CurSize, MaxSize, BackupCount
    ),
    State#state{report_fd=NewFd, report_size=NewSize};

%msg and unknown
rotate_if_needed(_Other, State, Size) ->
    FileName = State#state.log_file,
    FileFd = State#state.log_fd,
    CurSize = State#state.log_size,
    MaxSize = State#state.log_max_size,
    BackupCount = State#state.log_backup_count,
    {NewFd, NewSize} = rotate_if_needed(
        FileName, FileFd, Size, CurSize, MaxSize, BackupCount
    ),
    State#state{log_fd=NewFd, log_size=NewSize}.
    

rotate_if_needed(FileName, FileFd, Size, CurSize, MaxSize, BackupCount)
        when CurSize + Size >= MaxSize ->
    case FileFd of
        undefined ->
            ok;
        Fd ->
            ok = close_file(Fd)
    end,
    {ok, Filenames} = file:list_dir(filename:dirname(FileName)),
    LogFiles = [F || F <- Filenames,
        string:str(F, filename:basename(FileName) ++ ".") =:= 1],
    N = case length(LogFiles) of
        Len when Len >= BackupCount ->
            BackupCount - 1;
        Len ->
            Len
    end,
    rename_log_files(FileName, N),
    {ok, NewFd} = open_file(FileName),
    {NewFd, Size};

rotate_if_needed(FileName, undefined, Size, CurSize, _MaxSize, _BC) ->
    {ok, Fd} = open_file(FileName),
    {Fd, CurSize + Size};

rotate_if_needed(_FileName, FileFd, Size, CurSize, _MaxSize, _BC) ->
    {FileFd, CurSize+Size}.


open_file(FileName) ->
    file:open(FileName, [{delayed_write, 1024*100, 1000}, append]).


close_file(Fd) ->
    case file:close(Fd) of
        ok ->
            ok;
        {error, _} ->
            close_file(10, Fd)
    end.


close_file(N, Fd) when N > 0 ->
    case file:close(Fd) of
        ok ->
            ok;
        {error, _} ->
            close_file(N-1, Fd)
    end;
close_file(_, _) ->
    ok.



rename_log_files(Filename, 0) ->
    ok = file:rename(Filename, Filename ++ ".1");
rename_log_files(Filename, N) ->
    Source = Filename ++ "." ++ io_lib:write(N),
    Destination = Filename ++ "." ++ io_lib:write(N + 1),
    case file:rename(Source, Destination) of
        ok ->
            ok;
        {error, enoent} ->
            ok
    end,
    rename_log_files(Filename, N - 1).



log_level(fatality) ->
    log_level(all);
log_level(all) ->
    0;
log_level(warning) ->
    1;
log_level(error) ->
    2;
log_level(none) ->
    3;
log_level(_) ->
    4.



log_value(info_msg) ->
    0;
log_value(info_report) ->
    0;
log_value(warning_msg) ->
    1;
log_value(warning_report) ->
    1;
log_value(error) ->
    2;
log_value(error_report) ->
    2;
log_value(_) ->
    0.


log_checker(Type, State, Fun, Args) ->
    Level = State#state.log_level,
    Bool = log_value(Type) >= Level,
    if
        Bool ->
            apply(Fun, Args);
        true ->
            {ok, State}
    end.

