-module(test_vcs_plugin).

-behaviour(caterpillar_repository_plugin).

-export([init_plugin/1, terminate_plugin/1]).
-export([export/5, get_branches/2]).
-export([get_changelog/5, get_diff/5, get_revno/3, get_tag/4]).
-export([is_branch/3, is_repository/2]).
-export([custom_command/1]).
-export([custom_command/2]).


-type vcs_state()::term().
-type package()::filelib:dirname().
-type branch()::string().
-type revno()::term().

-spec init_plugin(Args::term()) -> {ok, vcs_state()}.
-spec terminate_plugin(vcs_state()) -> no_return().
-spec export(vcs_state(), package(), branch(), revno(), filelib:dirname()) -> ok | {error, Reason::term()}.
-spec get_branches(vcs_state(), package()) -> {ok, [branch()]} | {error, Reason::term()}.
-spec get_revno(vcs_state(), package(), branch()) -> {ok, revno()} | {error, Reason::term()}.
-spec get_diff(vcs_state(), package(), branch(), Old::revno(), Current::revno()) ->
    {ok, Diff::binary()} | {error, Reason::term()}.
-spec get_changelog(vcs_state(), package(), branch(), Old::revno(), Current::revno()) ->
    {ok, Changelog::binary()} | {error, Reason::term()}.
-spec is_branch(vcs_state(), package(), branch()) -> boolean().
-spec is_repository(vcs_state(), package()) -> boolean().



init_plugin(_Args) -> {ok, state}.


terminate_plugin(_State) -> ok.


export(_State, _Package, "no_export", _Revision, _ExportPath) -> error;
export(_State, _Package, _Branch, _Revision, ExportPath) -> caterpillar_utils:ensure_dir(ExportPath), ok.


get_branches(_State, "test_repo/sleep") -> timer:sleep(12000), {ok, []};
get_branches(_State, "__test/sleep") -> timer:sleep(5), {ok, []};
get_branches(_State, Package) -> caterpillar_utils:list_packages(Package).


get_changelog(_State, _Package, "branch1", _, _) -> {ok, <<"branch1 changelog">>};
get_changelog(_State, _Package, "branch2", _, _) -> exit(some_error);
get_changelog(_State, _Package, _Branch, _PrevRevision, _CurrentRevision) -> ok.


get_diff(_State, _Package, "branch1", _, _) -> {ok, <<"branch1 diff">>};
get_diff(_State, _Pacakge, "branch2", _, _) -> throw("error");
get_diff(_State, _Package, _Branch, _PrevRevision, _CurrentRevision) -> ok.


get_revno(_State, "__test/crash", "me") -> exit(some_reason);
get_revno(_State, _Package, _Branch) -> {ok, 1}.


get_tag(_State, _Package, "crash", _Revno) -> exit(error);
get_tag(_State, _Package, _Branch, _Revno) -> {ok, "tag"}.


is_branch(_State, _Package, "branch1") -> true;
is_branch(_State, "__test/package2", "branch2") -> true;
is_branch(_State, "__test/абв", "вба") -> true;
is_branch(_State, "__test/package1", "exit") -> exit(some_reason);
is_branch(_State, "__test/package2", "throw") -> throw(some_reason);
is_branch(_State, _Package, _Branch) -> false.


is_repository(_State, "test_repo/package1") -> true;
is_repository(_State, "test_repo/sleep") -> true;
is_repository(_State, "__test/абв") -> true;
is_repository(_State, "__test/sleep") -> true;
is_repository(_State, "__test/package1") -> true;
is_repository(_State, "__test/package2") -> true;
is_repository(_State, "__test/exit") -> exit(some_reason);
is_repository(_State, "__test/throw") -> throw(some_reason);
is_repository(_State, _Package) -> false.


custom_command(State) ->
    {'custom_command/1', State}.


custom_command(State, Arg) ->
    {'custom_command/2', State, Arg}.
