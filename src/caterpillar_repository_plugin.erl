-module(caterpillar_repository_plugin).

-export([behaviour_info/1]).
-compile(nowarn_unused_vars).


behaviour_info(callbacks) ->
    [
        {init_plugin, 1},
        {terminate_plugin, 1},
        {get_tag, 4},
        {get_diff, 5},
        {get_changelog, 5},
        {get_revno, 3},
        {is_repository, 2},
        {is_branch, 3},
        {get_branches, 2},
        {export_archive, 5}
    ];
behaviour_info(_) -> undefined.


-type vcs_state()::term().
-type name()::filelib:dirname().
-type branch()::string().
-type revno()::term().
-type archive_type()::zip|tar|tgz|atom().



-spec init_plugin(Args::term()) -> {ok, VcsState::vcs_state()}|{error, Reason::term()}.
init_plugin(Args) -> {ok, state}.


-spec terminate_plugin(vcs_state()) -> no_return().
terminate_plugin(PluginState) ->  ok.


-spec export_archive(vcs_state(), name(), branch(), revno(), ArchivePath::filelib:dirname()) ->
    {ok, Type::archive_type()}|{error, Reason::term()}.
export_archive(PluginState, Name, Branch, Revno, ArchivePath) -> {ok, tar}.


-spec get_branches(vcs_state(), name()) -> {ok, [branch()]} | {error, Reason::term()}.
get_branches(PluginState, Name) -> {ok, []}.


-spec get_revno(vcs_state(), name(), branch()) -> {ok, revno()} | {error, Reason::term()}.
get_revno(PluginState, Name, Branch) -> {ok, revno}.


-spec get_diff(vcs_state(), name(), branch(), revno(), revno()) -> {ok, Diff::binary()} | {error, Reason::term()}.
get_diff(PluginState, Name, Branch, Old, Current) -> {ok, <<>>}.



-spec get_changelog(vcs_state(), name(), branch(), revno(), revno()) -> {ok, Changelog::binary()} | {error, Reason::term()}.
get_changelog(PluginState, Name, Branch, Old, Current) -> {ok, <<>>}.


-spec is_branch(vcs_state(), name(), branch()) -> boolean().
is_branch(PluginState, Name, Branch) -> true.


-spec is_repository(vcs_state(), name()) -> boolean().
is_repository(PluginState, Name) -> true.
