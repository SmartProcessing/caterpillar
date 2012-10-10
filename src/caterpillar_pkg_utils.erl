-module(caterpillar_pkg_utils).
-include("caterpillar.hrl").
-export([get_pkg_config/1]).
-export([get_pkg_config_record/2]).
-export([get_dep_list/1]).
-export([pack_rev_def/2]).
-define(LTB, list_to_binary).

get_pkg_config_record(Archive, {control, Data}) ->
    #pkg_config{
        name=?GV("Package", Data, Archive#archive.name),
        version=?GV("Version", Data, "0.0.0"),
        section=?GV("Section", Data, "default"),
        package_t=["deb"],
        arch=?GV("Architecture", Data, "all"),
        maintainers=[?GV("Maintainer", Data, "example@example.org")],
        platform="default",
        deps=?GV("Depends", Data, [])
    };
get_pkg_config_record(Archive, {config, Data}) ->
    #pkg_config{
        name=?GV("name", Data, Archive#archive.name),
        version=?GV("version", Data, "0.0.0"),
        section=?GV("section", Data, "default"),
        package_t=?GV("package_t", Data, ["deb"]),
        arch=?GV("architecture", Data, "all"),
        maintainers=?GV("maintainers", Data, ["example@example.org"]),
        platform=?GV("platform", Data, "default"),
        deps=?GV("deps", Data, []),
        build_deps=?GV("build_deps", Data, [])
    }.

get_dep_list(Pkg) ->
    get_valid_versions(Pkg#pkg_config.deps) ++ 
        get_valid_versions(Pkg#pkg_config.build_deps).

get_valid_versions(L) ->
    [{?LTB(N), ?LTB(B), ?LTB(T)} || {N, B, T} <- L].


pack_rev_def(Archive, Deps) ->
    #rev_def{
        name=Archive#archive.name,
        branch=Archive#archive.branch,
        tag=Archive#archive.tag,
        dep_object=Deps
    }.


get_pkg_config(Path) ->
    case catch file:consult(Path ++ "/pkg.config") of
        {ok, Term} ->
            {config, Term};
        _Other ->
            parse_control(Path)
    end.

parse_control(Path) ->
    case catch file:read_file(Path ++ "/control") of
        {ok, Content} ->
            {control, lists:map(
                fun(Entry) ->
                    parse_control_entry(string:tokens(Entry, ":"))
                end,
                string:tokens(Content, "\n"))};
        {error, Reason} ->
            error_logger:error_msg("no control file: ~p~n", [Reason]),
            [];
        Other ->
            error_logger:error_msg("no control file: ~p~n", [Other]),
            []
    end.

parse_control_entry(["Depends", Deps]) ->
    {"Depends", lists:map(
        fun(Dep) ->
            {Package, _Version} = string:tokens(string:strip(Dep), "("),
            Package
        end, 
        string:tokens(Deps, ","))};
parse_control_entry([Param, Value]) ->
    {Param, string:strip(Value)}.
