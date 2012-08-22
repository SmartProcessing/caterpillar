-module(caterpillar_utils).
-include("caterpillar.hrl").

-export([get_version_by_revdef/1]).

-spec get_version_by_revdef(RevDef :: #rev_def{}) -> Version :: version().
get_version_by_revdef(RevDef) ->
    Name = RevDef#rev_def.name,
    Branch = RevDef#rev_def.branch,
    Tag = RevDef#rev_def.tag,
    {Name, Branch, Tag}.
