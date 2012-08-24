-module(caterpillar_api_handler).
-export([init/3, handle/2, terminate/2]).
-define(GV, proplists:get_value).

init({tcp, http}, Req, Opts) ->
    {ok, Req, Opts}.

handle(Req, State) ->
    {UrlArgs, _Req} = cowboy_http_req:qs_vals(Req),
    logging:info_msg("received caterpillar request: ~n~p", [UrlArgs]),
    {ok, Req2} = cowboy_http_req:reply(200, [], <<"">>, Req),
    catch handle_action(
        proplists:get_value(<<"action">>, UrlArgs), UrlArgs, State),
    {ok, Req2, State}.

handle_action(undefined, _Args, _Settings) ->
    logging:error_msg("no action specified in query"),
    {error, no_action};
handle_action(<<"newrepo">>, Args, Settings) ->
    Name = ?GV(<<"name">>, Args),
    PluginName = ?GV(<<"plugin">>, Args),
    Parent = ?GV(parent_pid, Settings),
    gen_server:call(Parent, {newrepo, PluginName, Name});
handle_action(Other, _Args, _Settings) ->
    logging:error_msg("invalid action: ~p", [Other]),
    {error, not_implemented}.

terminate(_Req, _State) ->
    ok.
