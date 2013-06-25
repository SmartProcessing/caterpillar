-module(caterpillar_storage).

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(DEFAULT_STORAGE, "/var/lib/smprc/caterpillar/storage").
-define(DTU, smprc_datetime_utils).

-include_lib("caterpillar.hrl").
-include_lib("caterpillar_storage.hrl").

-export([start_link/0, start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {storage='storage', registered=false}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_link(Settings) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Settings, []).

init(Settings) ->
    {ok, Storage} = dets:open_file(storage, [
            {file, ?GV(storage, Settings, ?DEFAULT_STORAGE)},
            {ram_file, true}]),
    async_register(),
    {ok, #state{storage=Storage, registered=false}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({store_start_build, [
            {Name, Branch},
            WorkId,
            CommitHash
        ]}, State=#state{storage=S}) ->
    case dets:lookup(S, {Name, Branch}) of
        [] ->
            dets:insert(S, {{Name, Branch}, "...", [WorkId]});
        [{_, Desc, Bids}] ->
            dets:insert(S, {{Name, Branch}, Desc, [WorkId|Bids]})
    end,
    dets:insert(S, {
            {WorkId, {Name, Branch}},
            <<"start">>,
            ?DTU:datetime_to_binary_string(calendar:universal_time()),
            <<"0000-00-00 00:00:00">>,
            CommitHash,
            <<"...">>,
            <<"...">>
        }),
    {noreply, State};

handle_cast({store_progress_build, [
            {Name, Branch},
            WorkId,
            Description,
            BuildMessage
        ]}, State=#state{storage=S}) ->
    case dets:lookup(S, {Name, Branch}) of
        [] ->
            dets:insert(S, {{Name, Branch}, Description, [WorkId]});
        [{_, _, Bids}] ->
            dets:insert(S, {{Name, Branch}, Description, [Bids]})
    end,
    case dets:lookup(S, {WorkId, {Name, Branch}}) of
        [{_, _, Start, End, CommitHash, _}] ->
            dets:insert(S, {
                    {WorkId, {Name, Branch}},
                    <<"in_progress">>,
                    Start,
                    End,
                    CommitHash,
                    <<"...">>,
                    <<"...">>
                });
        _ ->
            pass
    end,
    {noreply, State};

handle_cast({store_error_build, [
            {Name, Branch},
            WorkId,
            BuildLog
        ]}, State=#state{storage=S}) ->
    case dets:lookup(S, {WorkId, {Name, Branch}}) of
        [{_, _, Start, _, CommitHash, _}] ->
            dets:insert(S, {
                    {WorkId, {Name, Branch}},
                    <<"error">>,
                    Start,
                    ?DTU:datetime_to_binary_string(calendar:universal_time()),
                    CommitHash,
                    BuildLog,
                    <<"">>
                });
        _ ->
            pass
    end,
    {noreply, State};

handle_cast({store_complete_build, [
            complete,
            {Name, Branch},
            WorkId,
            Package,
            BuildLog
        ]}, State=#state{storage=S}) ->
    case dets:lookup(S, {WorkId, {Name, Branch}}) of
        [{_, _, Start, _, CommitHash, _}] ->
            dets:insert(S, {
                    {WorkId, {Name, Branch}},
                    <<"success">>,
                    Start,
                    ?DTU:datetime_to_binary_string(calendar:universal_time()),
                    CommitHash,
                    BuildLog,
                    Package
                });
        _ ->
            pass
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({'DOWN', _, _, _, _}, State) ->
    async_register(),
    {noreply, State#state{registered=false}};

handle_info(async_register, State=#state{registered=false}) ->
    case catch caterpillar_event:register_service(storage) of
        {ok, Pid} -> 
            erlang:monitor(process, Pid),
            {noreply, State#state{registered=true}};
        _ ->
            async_register(),
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%-----------------

async_register() -> 
    async_register(1000).

async_register(Delay) ->
    erlang:send_after(Delay, self(), async_register).
