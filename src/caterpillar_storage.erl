-module(caterpillar_storage).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(DEFAULT_STORAGE, "/var/lib/smprc/caterpillar/storage").
-define(GV, proplists:get_value).

-export([start_link/0, start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_link(Settings) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Settings, []).

init(Settings) ->
    {ok, Storage} = dets:open_file(storage, [
            {file, ?GV(storage, Settings, ?DEFAULT_STORAGE)},
            {ram_file, true}]),
    {ok, [{storage, Storage}]}.

handle_call({build_info, [
            Name, 
            Branch, 
            Tag, 
            WorkId, 
            CommitNum,
            Diff, 
            CommitMessage,
            BuildMessage
        ]}, _From, State) ->
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
