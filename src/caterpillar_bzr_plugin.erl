-module(caterpillar_bzr_plugin).

-behaviour(gen_server).
-behaviour(caterpillar_repository_plugin).

-export([init_plugin/1, terminate_plugin/1]).
-export([get_diff/5, get_changelog/5, get_revno/3]).
-export([is_repository/2, is_branch/3]).
-export([get_branches/2]).
-export([export/4]).


-export([init/1, terminate/2, handle_info/2, handle_cast/2, handle_call/3, code_change/3]).


-record(state, {port, buf, queue=queue:new()}).


-define(TIMEOUT, infinity).



init_plugin(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).


terminate_plugin(_State) ->
    gen_server:call(?MODULE, stop, infinity).
    

call_server(Msg) ->
    gen_server:call(?MODULE, Msg, ?TIMEOUT).



get_diff(PackagePath, Package, Branch, OldRevno, NewRevno) ->
    case call_server({get_diff, PackagePath, Package, Branch, OldRevno, NewRevno}) of
        {ok, BinDiff} ->
            DiffLength = size(BinDiff),
            Header = io_lib:format("Diff contains ~p bytes\n\n", [DiffLength]),
            NDiff = if 
                DiffLength > 10*1024 ->
                    io_lib:format("~10240s...\n", [BinDiff]);
                true ->
                    binary_to_list(BinDiff)
            end,
            {ok, lists:flatten(Header ++ NDiff)};
        {error, Error} ->
            {error, binary_to_list(Error)};
        _Oth ->
            {error, io_lib:format("bad response from get_diff ~p~n", [_Oth])}
    end.



get_changelog(PackagePath, Package, Branch, OldRevno, NewRevno) ->
    case call_server({get_changelog, PackagePath, Package, Branch,OldRevno,NewRevno}) of
        {ok, BinChanges} ->
            LenChanges = size(BinChanges),
            NChanges = if 
                LenChanges >= 10*1024 ->
                    io_lib:format("~10240s...\n", [BinChanges]);
                true ->
                    binary_to_list(BinChanges)
            end,
            {ok, lists:flatten(NChanges)};
        {error, Error} ->
            {error, binary_to_list(Error)};
        _Oth ->
            {error, io_lib:format("bad response from get_changelog ~p~n", [_Oth])}
    end.



get_revno(PackagePath, Package, Branch) ->
    call_server({get_revno, PackagePath, Package, Branch}).


get_branches(PackagePath, Package) ->
    case call_server({get_branches, PackagePath, Package}) of
        {ok, Branches} ->
            {ok, lists:map(fun binary_to_list/1, Branches)};
        Err ->
            Err
    end.


is_repository(PackagePath, Package) ->
    call_server({is_repository, PackagePath, Package}).


is_branch(PackagePath, Package, Branch) ->
    call_server({is_branch, PackagePath, Package, Branch}).


export(PackagePath, Package, Branch, ExportPath) ->
    call_server({export_branch, PackagePath, Package, Branch, ExportPath}).




%------------------




init(Args) ->
    error_logger:info_msg("caterpillar_bzr_plugin_server args: ~p~n", [Args]),
    BzrServer = proplists:get_value(bzr_server, Args, "priv/bzr_server.py"),
    Port = open_port(
        {spawn, BzrServer},
        [binary, nouse_stdio, stream, exit_status]
    ),
    {ok, #state{port=Port, buf = <<>>}}.


handle_info({_Port, {data, Data}}, #state{buf=Buf, queue=Queue}=State) ->
    Bin = <<Buf/binary, Data/binary>>,
    {NewBuf, NewQueue} = case handle_port_data(Bin, [], <<>>) of
        {no_message, SomeBuf} ->
            {SomeBuf, Queue};
        {Messages, SomeBuf} ->   
            SomeQueue = reply(Messages, Queue),
            {SomeBuf, SomeQueue}
    end,
    {noreply, State#state{buf=NewBuf, queue=NewQueue}};

handle_info({_Port, {exit_status, Status}}, State) ->
    error_logger:error_msg("port exited with status ~p~n", [Status]),
    {stop, port_exited, State};

handle_info(_Msg, State) ->
    error_logger:info_msg("flushing ~p~n", [_Msg]),
    {noreply, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.



handle_call(Msg, From, #state{port=Port, queue=Queue}=State) ->
    case call_port(Msg, Port) of
        deferred ->
            {noreply, State#state{queue=queue:in(From, Queue)}};
        Error ->
            {reply, Error, State}
    end;

handle_call(stop, _, State) ->
    {stop, normal, ok, State};

handle_call(_, _, State) ->
    {reply, {error, bad_msg}, State}.


terminate(Reason, State) ->
    error_logger:info_msg("caterpillar_bzr_plugin terminated with reason: ~p~n", [Reason]),
    catch erlang:port_close(State#state.port),
    ok.


code_change(_Old, State, _Extra) ->
    {ok, State}.



call_port({get_revno, PackagePath, Package, Branch}, Port) ->
    Msg = [
        {cmd, get_revno}, 
        {repo_path, list_to_binary(PackagePath)},
        {repo, list_to_binary(Package)},
        {branch, list_to_binary(Branch)}
    ],
    erlang:port_command(Port, encode(Msg)),
    deferred;

call_port({get_changelog, PackagePath, Package, Branch, OldRevno, NewRevno}, Port) ->
    Msg = [
        {cmd, get_changelog}, 
        {repo_path, list_to_binary(PackagePath)},
        {repo, list_to_binary(Package)},
        {branch, list_to_binary(Branch)},
        {old_revno, OldRevno},
        {new_revno, NewRevno}
    ],
    erlang:port_command(Port, encode(Msg)),
    deferred;

call_port({get_diff, PackagePath, Package, Branch, OldRevno, NewRevno}, Port) ->
    Msg = [
        {cmd, get_diff}, 
        {repo_path, list_to_binary(PackagePath)},
        {repo, list_to_binary(Package)},
        {branch, list_to_binary(Branch)},
        {old_revno, OldRevno},
        {new_revno, NewRevno}
    ],
    erlang:port_command(Port, encode(Msg)),
    deferred;

call_port({checkout_branch, PackagePath, Package, Branch}, Port) ->
    Msg = [
        {cmd, checkout_branch}, 
        {repo_path, list_to_binary(PackagePath)},
        {repo, list_to_binary(Package)},
        {branch, list_to_binary(Branch)}
    ],
    erlang:port_command(Port, encode(Msg)),
    deferred;

call_port({is_repository, PackagePath, Package}, Port) ->
    Msg = [
        {cmd, is_repository}, 
        {repo_path, list_to_binary(PackagePath)},
        {repo, list_to_binary(Package)}
    ],
    erlang:port_command(Port, encode(Msg)),
    deferred;

call_port({is_branch, PackagePath, Package, Branch}, Port) ->
    Msg = [
        {cmd, is_branch}, 
        {repo_path, list_to_binary(PackagePath)},
        {repo, list_to_binary(Package)},
        {branch, list_to_binary(Branch)}
    ],
    erlang:port_command(Port, encode(Msg)),
    deferred;

call_port({get_branches, PackagePath, Package}, Port) ->
    Msg = [
        {cmd, get_branches}, 
        {repo_path, list_to_binary(PackagePath)},
        {repo, list_to_binary(Package)}
    ],
    erlang:port_command(Port, encode(Msg)),
    deferred;

call_port({export_branch, PackagePath, Package, Branch, ExportPath}, Port) ->
    Msg = [
        {cmd, export_branch}, 
        {repo_path, list_to_binary(PackagePath)},
        {repo, list_to_binary(Package)},
        {branch, list_to_binary(Branch)},
        {export_path, list_to_binary(ExportPath)}
    ],
    erlang:port_command(Port, encode(Msg)),
    deferred;

call_port(_, _) ->
    {error, bad_msg}.



encode(Msg) ->
    Bin = list_to_binary(mochijson2:encode({struct, Msg})),
    <<Bin/binary, 0:8>>.


decode(Msg) ->
    {struct, [Result]} = mochijson2:decode(Msg),
    case Result of
        {<<"error">>, Error} ->
            {error, Error};
        {<<"ok">>, Res} ->
            {ok, Res}
    end.


handle_port_data(<<0:8, Binary/binary>>, Accum, Buf) ->
    handle_port_data(Binary, [Buf|Accum], <<>>);
handle_port_data(<<Char:8, Binary/binary>>, Accum, Buf) ->
    handle_port_data(Binary, Accum, <<Buf/binary, Char:8>>);
handle_port_data(<<>>, [], Buf) ->
    {no_message, Buf};
handle_port_data(<<>>, Accum, Buf) ->
    NewAccum = lists:foldl(
        fun(<<>>, Acc) -> Acc;(Msg, Acc) -> [decode(Msg)|Acc] end,
        [],
        Accum
    ),
    {lists:reverse(NewAccum), Buf}.



reply([Msg|O], Queue) ->
    {{value, Item}, NewQueue} = queue:out(Queue),
    gen_server:reply(Item, Msg),
    reply(O, NewQueue);
reply([], Queue) ->
    Queue.

