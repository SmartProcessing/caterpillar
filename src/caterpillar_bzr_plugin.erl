-module(caterpillar_bzr_plugin).

-behaviour(gen_server).
-behaviour(caterpillar_repository_plugin).

-export([init_plugin/1, terminate_plugin/1]).
-export([get_diff/5, get_changelog/5, get_revno/3]).
-export([is_repository/2, is_branch/3]).
-export([get_branches/2]).
-export([export/5]).
-export([get_tag/4]).


-export([init/1, terminate/2, handle_info/2, handle_cast/2, handle_call/3, code_change/3]).


-record(state, {port, buf, queue=queue:new()}).


-define(TIMEOUT, infinity).



init_plugin(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).


terminate_plugin(_State) ->
    gen_server:call(?MODULE, stop, infinity).
    

call_server(Msg) ->
    gen_server:call(?MODULE, Msg, ?TIMEOUT).



get_diff(_State, Package, Branch, OldRevno, NewRevno) ->
    call_server({get_diff, Package, Branch, OldRevno, NewRevno}).



get_changelog(_State, Package, Branch, OldRevno, NewRevno) ->
    call_server({get_changelog, Package, Branch,OldRevno,NewRevno}).



get_revno(_State, Package, Branch) ->
    call_server({get_revno, Package, Branch}).


get_branches(_State, Package) ->
    case call_server({get_branches, Package}) of
        {ok, Branches} ->
            {ok, lists:map(fun binary_to_list/1, Branches)};
        Err ->
            Err
    end.


get_tag(_State, _Package, _Branch, _Revno) ->
    {ok, tag}.


is_repository(_State, Package) ->
    case call_server({is_repository, Package}) of
        {ok, _} -> true;
        _ -> false
    end.


is_branch(_State, Package, Branch) ->
    case call_server({is_branch, Package, Branch}) of
        {ok, _} -> true;
        _ -> false
    end.


export(_State, Package, Branch, _Revno, ExportPath) ->
    %FIXME: Revno
    case call_server({export_branch, Package, Branch, ExportPath}) of
        {ok, _} -> ok;
        Err -> Err
    end.




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



call_port({get_revno, Package, Branch}, Port) ->
    Msg = [
        {cmd, get_revno}, 
        {repo, list_to_binary(Package)},
        {branch, list_to_binary(Branch)}
    ],
    erlang:port_command(Port, encode(Msg)),
    deferred;

call_port({get_changelog, Package, Branch, OldRevno, NewRevno}, Port) ->
    Msg = [
        {cmd, get_changelog}, 
        {repo, list_to_binary(Package)},
        {branch, list_to_binary(Branch)},
        {old_revno, OldRevno},
        {new_revno, NewRevno}
    ],
    erlang:port_command(Port, encode(Msg)),
    deferred;

call_port({get_diff, Package, Branch, OldRevno, NewRevno}, Port) ->
    Msg = [
        {cmd, get_diff}, 
        {repo, list_to_binary(Package)},
        {branch, list_to_binary(Branch)},
        {old_revno, OldRevno},
        {new_revno, NewRevno}
    ],
    erlang:port_command(Port, encode(Msg)),
    deferred;

call_port({checkout_branch, Package, Branch}, Port) ->
    Msg = [
        {cmd, checkout_branch}, 
        {repo, list_to_binary(Package)},
        {branch, list_to_binary(Branch)}
    ],
    erlang:port_command(Port, encode(Msg)),
    deferred;

call_port({is_repository, Package}, Port) ->
    Msg = [
        {cmd, is_repository}, 
        {repo, list_to_binary(Package)}
    ],
    erlang:port_command(Port, encode(Msg)),
    deferred;

call_port({is_branch, Package, Branch}, Port) ->
    Msg = [
        {cmd, is_branch}, 
        {repo, list_to_binary(Package)},
        {branch, list_to_binary(Branch)}
    ],
    erlang:port_command(Port, encode(Msg)),
    deferred;

call_port({get_branches, Package}, Port) ->
    Msg = [
        {cmd, get_branches}, 
        {repo, list_to_binary(Package)}
    ],
    erlang:port_command(Port, encode(Msg)),
    deferred;

call_port({export_branch, Package, Branch, ExportPath}, Port) ->
    Msg = [
        {cmd, export_branch}, 
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

