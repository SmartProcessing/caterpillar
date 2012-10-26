-module(caterpillar_pkg_utils).
-include("caterpillar.hrl").
-include("caterpillar_internal.hrl").
-export([get_pkg_config/2]).
-export([get_pkg_config_record/2]).
-export([get_dep_list/1]).
-export([pack_rev_def/2, get_dir_name/1]).
-define(LTB, list_to_binary).
-define(BTL, binary_to_list).

get_pkg_config_record(Archive, {control, Data}) ->
    #pkg_config{
        name=?GV("Package", Data, Archive#archive.name),
        version=?GV("Version", Data, "0.0.0"),
        section=?GV("Section", Data, "smprc"),
        package_t=["deb"],
        arch=?GV("Architecture", Data, "all"),
        maintainers=[?GV("Maintainer", Data, "example@example.org")],
        platform="default",
        deps=?GV("Depends", Data, [])
    };
get_pkg_config_record(Archive, {config, Data}) ->
    error_logger:info_msg("BUILD DEPS! ~p~n", [?GV(build_deps, Data)]),
    #pkg_config{
        name=?GV(name, Data, Archive#archive.name),
        version=?GV(version, Data, "0.0.0"),
        section=?GV(section, Data, "smprc"),
        package_t=?GV(package_t, Data, ["deb"]),
        arch=?GV(architecture, Data, "all"),
        maintainers=?GV(maintainers, Data, ["example@example.org"]),
        platform=?GV(platform, Data, "default"),
        deps=?GV(deps, Data, []),
        build_deps=?GV(build_deps, Data, [])
    };
get_pkg_config_record(Archive, {empty, Data}) ->
    #pkg_config{
        name=?GV("name", Data, Archive#archive.name),
        version=?GV("version", Data, "0.0.0"),
        section=?GV("section", Data, "smprc"),
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


pack_rev_def(Archive, PkgRecord) ->
    Deps = get_dep_list(PkgRecord),
    #rev_def{
        name=?LTB(Archive#archive.name),
        branch=?LTB(Archive#archive.branch),
        tag=?LTB(Archive#archive.tag),
        pkg_config = PkgRecord,
        dep_object=Deps
    }.


get_pkg_config_list(Path) ->
    case [
            catch file:consult(filename:join(Path, "pkg.config")),
            filelib:is_file(Path ++ "control") ] of
        [{ok, [Term]}|_] ->
            {config, Term};
        [{error, _}, true] ->
            {control, parse_control(Path)};
        _Other ->
            {empty, []}
    end.


get_pkg_config(Arch, Path) ->
    Res = get_pkg_config_record(Arch, get_pkg_config_list(Path)),
    logging:info_msg("got package config record: ~p~n", [Res]),
    Res.


get_dir_name(Rev) ->
    Name = ?BTL(Rev#rev_def.name),
    Branch = ?BTL(Rev#rev_def.branch),
    Tag = ?BTL(Rev#rev_def.tag),
    io_lib:format(
        "~s-~s~s", [Name, Branch, Tag]).

parse_control(Path) ->
    case catch file:read_file(filename:join([Path, "control"])) of
        {ok, Content} ->
            lists:map(
                fun(Entry) ->
                    parse_control_entry(string:tokens(Entry, ":"))
                end,
                string:tokens(?BTL(Content), "\n"));
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
            [Package|_] = string:tokens(string:strip(Dep), "("),
            Package
        end, 
        string:tokens(Deps, ","))};
parse_control_entry([Param, Value]) ->
    {Param, string:strip(Value)}.
