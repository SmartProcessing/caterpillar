-module(test_vcs_plugin).

-behaviour(caterpillar_repository_plugin).

-export([init/1, terminate/1]).
-export([export/4, get_branches/2]).
-export([get_changelog/5, get_diff/5, get_revno/3]).
-export([is_branch/3, is_repository/2]).


init(_Args) -> {ok, state}.


terminate(_State) -> ok.


export(_State, _Repo, "no_export", _ExportPath) -> error;
export(_State, _Repo, _Branch, ExportPath) -> filelib:ensure_dir(ExportPath ++ "/").


get_branches(_State, _Repo) -> [].


get_changelog(_State, _Repo, _Branch, _PrevRevision, _CurrentRevision) -> ok.


get_diff(_State, _Repo, _Branch, _PrevRevision, _CurrentRevision) -> ok.


get_revno(_State, "__test/crash", "me") -> exit(some_reason);
get_revno(_State, _Package, _Branch) -> {ok, 1}.


is_branch(_State, "__test/package1", "branch1") -> true;
is_branch(_State, "__test/package2", "branch2") -> true;
is_branch(_State, "__test/package1", "exit") -> exit(some_reason);
is_branch(_State, "__test/package2", "throw") -> throw(some_reason);
is_branch(_State, _Package, _Branch) -> throw({_Package, _Branch}),false.


is_repository(_State, "__test/package1") -> true;
is_repository(_State, "__test/package2") -> true;
is_repository(_State, "__test/exit") -> exit(some_reason);
is_repository(_State, "__test/throw") -> throw(some_reason);
is_repository(_State, _Repo) -> false.
