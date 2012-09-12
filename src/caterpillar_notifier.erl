-module(caterpillar_notifier).

-export([notify/4]).

notify(_Who, _From, _Message, _Tools) ->
    {error, not_implemented}.
