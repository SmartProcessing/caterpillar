-module(caterpillar_pkg_utils).
-include("caterpillar.hrl").
-include("caterpillar_builder_internal.hrl").
-export([get_pkg_config/2]).
-export([get_pkg_config_record/2]).
-export([get_dep_list/2]).
-export([get_archive_version/1]).
-export([get_version_archive/1]).
-export([pack_rev_def/3, get_dir_name/1]).
-export([gen_control_from_pkg_config/1]).
-define(LTB, list_to_binary).
-define(BTL, binary_to_list).
-define(ITL, integer_to_list).

get_pkg_config_record(Archive, {control, Data}) ->
    #pkg_config{
        name=?GV("Package", Data, Archive#archive.name),
        version=?GV("Version", Data, "0.0.0"),
        section=?GV("Section", Data, "smprc"),
        package_t=["deb"],
        arch=?GV("Architecture", Data, "all"),
        description=?GV("Description", Data, Archive#archive.name),
        maintainers=[?GV("Maintainer", Data, ["example@example.org"])],
        platform="default",
        deps=?GV("Depends", Data, [])
    };
get_pkg_config_record(Archive, {config, Data}) ->
    #pkg_config{
        name=?GV(name, Data, Archive#archive.name),
        version=?GV(version, Data, "0.0.0"),
        section=?GV(section, Data, "smprc"),
        package_t=?GV(package_plugin, Data, ["deb"]),
        arch=?GV(architecture, Data, "all"),
        maintainers=?GV(maintainers, Data, ["example@example.org"]),
        description=?GV(description, Data, Archive#archive.name),
        platform=?GV(platform_plugin, Data, "default"),
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
        description=?GV("description", Data, Archive#archive.name),
        platform=?GV("platform", Data, "default"),
        deps=?GV("deps", Data, []),
        build_deps=?GV("build_deps", Data, [])
    };
get_pkg_config_record(_Archive, {error, Reason}) ->
    error_logger:info_msg("failed to parse pkg.config"),
    {error, Reason}.


get_dep_list(Pkg, Archive) ->
    get_valid_versions(Pkg#pkg_config.deps, Archive, deps) ++ 
        get_valid_versions(Pkg#pkg_config.build_deps, Archive, build_deps).

get_valid_versions(L, Archive, deps) ->
    Res = [{?LTB(N), ?LTB(B), ?LTB(T)} || {N, B, T} <- L],
    lists:map(fun({N, B, T}) ->
        case B of
            <<"*">> ->
                {{N, ?LTB(Archive#archive.branch), T}, <<"built">>};
            RealBranch ->
                {{N, RealBranch, T}, <<"built">>}
        end
    end, Res);
get_valid_versions(L, Archive, build_deps) ->
    lists:map(fun
    ({N, B, T}) ->
        case B of
            "*" ->
                {{?LTB(N), ?LTB(Archive#archive.branch), ?LTB(T)}, <<"new">>};
            RealBranch ->
                {{?LTB(N), ?LTB(RealBranch), ?LTB(T)}, <<"new">>}
        end;
    ({{N, B, T}, State}) ->
        case B of
            "*" ->
                {{?LTB(N), ?LTB(Archive#archive.branch), ?LTB(T)}, ?LTB(State)};
            RealBranch ->
                {{?LTB(N), ?LTB(RealBranch), ?LTB(T)}, ?LTB(State)}
        end
    end, L).

get_archive_version(Archive) ->
    {
        ?LTB(Archive#archive.name),
        ?LTB(Archive#archive.branch),
        ?LTB(Archive#archive.tag)
    }.

get_version_archive({Name, Branch, Tag}) ->
    #archive{
        name = ?BTL(Name),
        branch = ?BTL(Branch),
        tag = ?BTL(Tag)
    }.

pack_rev_def(Archive, PkgRecord, WorkId) ->
    Deps = get_dep_list(PkgRecord, Archive),
    #rev_def{
        name=?LTB(Archive#archive.name),
        branch=?LTB(Archive#archive.branch),
        tag=?LTB(Archive#archive.tag),
        work_id=WorkId,
        pkg_config = PkgRecord,
        dep_object=Deps
    }.


get_pkg_config_list(Path) ->
    case [
            catch file:consult(filename:join([Path, "pkg.config"])),
            filelib:is_file(filename:join([Path, "control"]))] of
        [{ok, [Term]}|_] ->
            {config, Term};
        [{error, enoent}, true] ->
            {control, parse_control(Path)};
        [{error, enoent}, false] ->
            {empty, []};
        [{error, Reason}, _] ->
            {error, Reason};
        _Other ->
            {empty, []}
    end.


get_pkg_config(Arch, Path) ->
    get_pkg_config_record(Arch, get_pkg_config_list(Path)).


get_dir_name(Rev=#rev_def{}) ->
    Name = ?BTL(Rev#rev_def.name),
    Branch = ?BTL(Rev#rev_def.branch),
    Tag = ?BTL(Rev#rev_def.tag),
    io_lib:format(
        "~s-~s~s", [Name, Branch, Tag]);
get_dir_name({N, B, T}) ->
    Name = ?BTL(N),
    Branch = ?BTL(B),
    Tag = ?BTL(T),
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
parse_control_entry([Param, Value|_]) ->
    {Param, string:strip(Value)}.


gen_control_from_pkg_config(Rev) ->
    PkgConfig = Rev#rev_def.pkg_config,
    OldVersion = PkgConfig#pkg_config.version,
    NewVersion = OldVersion ++ "-" ++ ?BTL(Rev#rev_def.branch) ++ "." ++ ?ITL(Rev#rev_def.work_id),
    OldSection = PkgConfig#pkg_config.section,
    NewSection = OldSection ++ "-" ++ ?BTL(Rev#rev_def.branch),
    [Maintainer|_] = PkgConfig#pkg_config.maintainers ++ ["example@example.org"],
    list_to_binary(io_lib:format(
        "Package: ~s~nSection: ~s~nVersion: ~s~nArchitecture: ~s~nDescription: ~s~nMaintainer: ~s~n~s",
        [
            PkgConfig#pkg_config.name,
            NewSection,
            NewVersion,
            PkgConfig#pkg_config.arch,
            PkgConfig#pkg_config.description,
            Maintainer,
            gen_deps(PkgConfig#pkg_config.deps)
        ])).

gen_deps([]) ->
    "";
gen_deps(Deps) ->
    gen_deps(Deps, []).
gen_deps([], Acc) ->
    "Depends: " ++ string:join(Acc, ", ") ++ "\n";
gen_deps([Dep|O], Acc) ->
    case Dep of
        Str when is_list(Dep) ->
            gen_deps(O, [Str|Acc]);
        {P, _B, _T} ->
            gen_deps(O, [P|Acc]);
        _Other ->
            gen_deps(O, Acc)
    end.
