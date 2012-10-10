-module(caterpillar_pkg_utils).
-include("caterpillar.hrl").
-export(get_pkg_config/1).
-export(get_pkg_record/2).

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
            [];
        Other ->
            []
    end.

parse_control_entry(["Depends", Deps]) ->
    {"Depends", lists:map(
        fun(Dep) ->
            {Package, Version} = string:tokens(string:strip(Dep), "("),
            Package
        end, 
        string:tokens(Deps, ","))};
parse_control_entry([Param, Value]) ->
    {Param, string:strip(Value)}.
