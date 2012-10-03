-module(test_vcs_plugin).

-behaviour(caterpillar_repository_plugin).

-export([init/1, terminate/1]).
-export([checkout_branch/3, export_branch/4, get_branches/2]).
-export([get_changelog/5, get_diff/5, get_revno/3]).
-export([is_branch/3, is_repository/2]).


init(_Args) -> {ok, state}.


terminate(_State) -> ok.


checkout_branch(_State, _Repo, _Branch) -> ok.


export_branch(_State, _Repo, _Branch, _ExportPath) -> ok.


get_branches(_State, _Repo) -> [].


get_changelog(_State, _Repo, _Branch, _PrevRevision, _CurrentRevision) -> ok.


get_diff(_State, _Repo, _Branch, _PrevRevision, _CurrentRevision) -> ok.


get_revno(_State, _Repo, _Branch) -> ok.


is_branch(_State, _Repo, _Branch) -> ok.


is_repository(_State, "__test/package1") -> true;
is_repository(_State, "__test/package2") -> true;
is_repository(_State, "__test/exit") -> exit(some_reason);
is_repository(_State, "__test/throw") -> throw(some_reason);
is_repository(_State, _Repo) -> false.
