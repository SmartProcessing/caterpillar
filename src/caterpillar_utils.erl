-module(caterpillar_utils).
-include("caterpillar.hrl").

-export([get_version_by_revdef/1, build_pipe/2]).

-type function_spec()   :: {function(), [term()]}.

-spec get_version_by_revdef(RevDef :: #rev_def{}) -> Version :: version().
get_version_by_revdef(RevDef) ->
    Name = RevDef#rev_def.name,
    Branch = RevDef#rev_def.branch,
    Tag = RevDef#rev_def.tag,
    {Name, Branch, Tag}.

-spec build_pipe([function_spec()], term()) -> {ok, term()} | {error, term()}.
build_pipe(Funs, Init) ->
    lists:foldl(
        fun({Function, Opts}, Acc) ->
            {ok, Res} = erlang:apply(Function, [Opts, Acc]),
            Res
        end, Init, Funs).

