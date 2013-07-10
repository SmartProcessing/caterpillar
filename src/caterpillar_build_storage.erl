-module(caterpillar_build_storage).
-include_lib("caterpillar_builder_internal.hrl").
-export([check_isect/2, list_unres_deps/3, list_buckets/2, empty_state_buckets/2, cleanup_new_in_progress/1]).
-export([update_dep_state/3, fetch_dep/2, fetch_dep_non_block/2, create_dep/2, get_subj/2]).
-export([create_bucket/2, find_bucket/2, arm_bucket/5]).
-export([fetch_bucket/2, update_dep_buckets/6, update_buckets/5, update_buckets/6, delete_from_bucket/4]).
-export([delete/4, get_path/3, get_statpack_path/3, get_temp_path/2]).

-define(LOCK(X), gen_server:call(caterpillar_lock, {lock, X}, infinity)).
-define(UNLOCK(X), gen_server:call(caterpillar_lock, {unlock, X})).
-define(CPU, caterpillar_pkg_utils).
-define(CU, caterpillar_utils).
-define(BTL, binary_to_list).

-spec list_unres_deps(reference(), #rev_def{}, [version()]) ->
    {ok, Unresolved :: [version()]}.
list_unres_deps(DepTree, Candidate, Preparing) ->
    Deps = Candidate#rev_def.dep_object,
    list_none_new_dependencies(DepTree, Deps, Preparing).

list_none_new_dependencies(DepTree, Deps, Preparing) ->
    list_none_new_dependencies(DepTree, Deps, Preparing, {[], []}).

list_none_new_dependencies(_DepTree, [], _Preparing, {NoneDeps, NewDeps}) ->
    {ok, NoneDeps, NewDeps};
list_none_new_dependencies(DepTree, [{Dep, State}|O], Preparing, {NoneDeps, NewDeps}) ->
    case fetch_dep(DepTree, Dep) of
        {ok, {_VersionSpec, {<<"missing">>, B}, _Obj, _Subj}} ->
            list_none_new_dependencies(DepTree, O, Preparing, {[Dep|NoneDeps], NewDeps});
        {ok, {_VersionSpec, {_Success, _B}, _Obj, _Subj}} when State == <<"new">> ->
            list_none_new_dependencies(DepTree, O, Preparing, {NoneDeps, NewDeps});
        {ok, {_VersionSpec, {<<"none">>, _B}, _Obj, _Subj}} ->
            list_none_new_dependencies(DepTree, O, Preparing, {[Dep|NoneDeps], NewDeps});
        {ok, {_VersionSpec, {NewInProgress, _B}, _Obj, _Subj}} when NewInProgress == <<"new">>; NewInProgress == <<"in_progress">> ->
            list_none_new_dependencies(DepTree, O, Preparing, {NoneDeps, [Dep|NewDeps]});
        {ok, {_VersionSpec, {Success, _B}, _Obj, _Subj}} when Success == <<"built">>; Success == State ->
            list_none_new_dependencies(DepTree, O, Preparing, {NoneDeps, NewDeps});
        {ok, []} ->
            case lists:member(Dep, Preparing) of
                true ->
                    list_none_new_dependencies(DepTree, O, Preparing, {NoneDeps, [Dep|NewDeps]});
                false ->
                    list_none_new_dependencies(DepTree, O, Preparing, {[Dep|NoneDeps], NewDeps})
            end;
        _Other ->
            list_none_new_dependencies(DepTree, O, Preparing, {[Dep|NoneDeps], NewDeps})
    end.


-spec check_isect(#rev_def{}, [#rev_def{}]) -> 
    {ok, independent|dependent}.
%% @doc Check whether the packages that are building now 
%% have in dependecies or depends on current build candidate
check_isect(Candidate, CheckList) ->
    CandidateVersion = ?VERSION(Candidate),
    CandidateDeps = [{N, B, T} || {{N, B, T}, _S} <- Candidate#rev_def.dep_object],
    Intersect = lists:foldl(
        fun(BuildUnit, Res) ->
            BuildVersion = ?VERSION(BuildUnit), 
            BuildDeps = [{N, B, T} || {{N, B, T}, _S} <- BuildUnit#rev_def.dep_object],
            IsDepObject = lists:member(CandidateVersion, BuildDeps),
            IsDepSubject = lists:member(BuildVersion, CandidateDeps),
            IsBuilding = BuildVersion =:= CandidateVersion,
            Res or IsDepObject or IsDepSubject or IsBuilding
        end, false, CheckList),
    case Intersect of
        true ->
            {ok, dependent};
        false ->
            {ok, independent}
    end.

-spec fetch_dep(DepTree :: reference(), Version :: version()) ->
    {ok, dependencie_record()} | {error, Reason :: atom()}.
fetch_dep(DepTree, Version) ->
    ?LOCK(Version),
    Res = case catch dets:lookup(DepTree, Version) of
        [{BuildVersion, State, Object, Subject}|_] ->
            {ok, {BuildVersion, State, Object, Subject}};
        [] ->
            {ok, []};
        _Other ->
            {error, dets_error}
    end,
    ?UNLOCK(Version),
    Res.

fetch_dep_non_block(DepTree, Version) ->
    case catch dets:lookup(DepTree, Version) of
        [{BuildVersion, State, Object, Subject}|_] ->
            {ok, {BuildVersion, State, Object, Subject}};
        [] ->
            {ok, []};
        _Other ->
            {error, dets_error}
    end.

get_subj(DepTree, Version) ->
    case fetch_dep(DepTree, Version) of
        {ok, {_, _, _, Subj}} ->
            Subj;
        _Other ->
            {error, not_found}
    end.

create_dep(DepTree, Rev) ->
    Version = ?VERSION(Rev),
    DepObject = Rev#rev_def.dep_object, 
    update_subjects(DepTree, DepObject, Version),
    ?LOCK(Version),
    case dets:lookup(DepTree, Version) of
        [{_V, {<<"missing">>, _}, _, Subj}|_] ->
            dets:insert(DepTree, {Version, {<<"new">>, []}, DepObject, Subj});
        [] ->
            dets:insert(DepTree, {Version, {<<"new">>, []}, DepObject, []});
        [{_V, {_, _Buckets}, _, _Subj}|_] ->
            pass
    end,
    ?UNLOCK(Version),
    {ok, done}.

-spec update_dep_state(reference(), #rev_def{}, binary()) -> 
    {ok, done}|{error, Reason :: atom()}.
update_dep_state(DepTree, Rev, Status) ->
    Version = ?VERSION(Rev),
    DepObject = Rev#rev_def.dep_object, 
    update_subjects(DepTree, DepObject, Version),
    ?LOCK(Version),
    NewObj = case dets:lookup(DepTree, Version) of
        [{V, {_, Buckets}, _, Subj}|_] when V == Version ->
            {Version, {Status, Buckets}, DepObject, Subj};
        [] ->
            {Version, {Status, []}, DepObject, []}
    end,
    dets:insert(DepTree, NewObj),
    ?UNLOCK(Version),
    {ok, done}.


update_subjects(_, [], _) ->
    {ok, done};
update_subjects(Deps, [{Version, _}|Other], NewRef) ->
    ?LOCK(Version),
    case dets:lookup(Deps, Version) of
        [{Version, BuildInfo, Object, Subject}|_] ->
            dets:insert(Deps, {Version, BuildInfo, Object, lists:usort([NewRef|Subject])});
        [] ->
            dets:insert(Deps, {Version, {<<"missing">>, []}, [], [NewRef]})
    end,
    ?UNLOCK(Version),
    update_subjects(Deps, Other, NewRef).

delete(Deps, Buckets, Path, Version) ->
    {Name, _, _} = Version,
    InBuckets = case fetch_dep(Deps, Version) of
        {ok, {Version, {_, In}, _, _}} ->
            dets:delete(Deps, Version),
            In;
        _Other ->
            []
    end,
    error_logger:info_msg("removing from buckets: ~p~n", [InBuckets]),
    BucketDel = fun(X) ->
        ?LOCK(X),
        [{BName, BPath, BContain}] = dets:lookup(Buckets, X), 
        dets:insert(Buckets, {BName, BPath, lists:delete(Version, BContain)}),
        ?UNLOCK(X),
        ?CU:del_dir(filename:join([Path, BPath, binary_to_list(Name)]))
    end,
    lists:map(BucketDel, InBuckets).


-spec create_bucket(reference(), #rev_def{}) ->
    {ok, BucketRef :: term()} | {error, Reason :: term()}.
% @doc Creates a directory pool for building some packages
create_bucket(BucketsDets, Rev) ->
    Version = ?VERSION(Rev),
    BucketId = get_new_bucket(BucketsDets),
    Bucket = {list_to_binary(BucketId), BucketId, [Version]},
    {ok, Bucket}.

get_new_bucket(Buckets) ->
    ?LOCK(last_bucket),
    LastBucket = dets:lookup(Buckets, last_bucket),
    Res = case LastBucket of
        [{last_bucket, Num}|_] ->
            ok = dets:insert(Buckets, {last_bucket, Num+1}),
            lists:flatten(io_lib:format("~4..0B", [Num + 1]));
        [] ->
            ok = dets:insert(Buckets, {last_bucket, 0}),
            lists:flatten(io_lib:format("~4..0B", [0]))
    end,
    ?UNLOCK(last_bucket),
    Res.

-spec find_bucket(reference(), #rev_def{}) ->
     [BucketRef :: term()] | [] | {error, term()}.
% @doc Find a bucket suitable for current version of the package.
find_bucket(BucketsDB, Rev) ->
    Version = ?VERSION(Rev),
    Deps = [{N, B, T} || {{N, B, T}, _} <- Rev#rev_def.dep_object],
    find_bucket(BucketsDB, Version, Deps).

find_bucket(BucketsDB, Version, Deps) ->
    iter_dets(BucketsDB, Version, Deps).

iter_dets(BucketDets, Version, Deps) ->
    First = dets:first(BucketDets),
    iter_dets(BucketDets, Version, Deps, First).

iter_dets(_BucketDets, _Version, _Deps, '$end_of_table') ->
    [];
iter_dets(BucketDets, Version, Deps, BucketId) ->
    ?LOCK(BucketId),
    B = dets:lookup(BucketDets, BucketId),
    case B of
        [{Id, Path, Entries}] = [Bucket] ->
            case validate_bucket(Entries, [Version|Deps]) of
                true ->
                    dets:insert(BucketDets, {Id, Path, lists:usort([Version|Entries] ++ Deps)}),
                    ?UNLOCK(BucketId),
                    [Bucket];
                false ->
                    ?UNLOCK(BucketId),
                    iter_dets(BucketDets, Version, Deps, dets:next(BucketDets, BucketId))
            end;
        _Other ->
            ?UNLOCK(BucketId),
            iter_dets(BucketDets, Version, Deps, dets:next(BucketDets, BucketId))
    end.

validate_bucket(Entries, Deps) ->
    validate_bucket(Entries, Deps, true).
validate_bucket([], _Deps, Prev) ->
    Prev;
validate_bucket([E|O], Deps, Prev) -> 
    {Name, Branch, Tag} = E,
    Res = case [{B, T} || {N, B, T} <- Deps, N == Name] of
        [{B, T}|_] when B /= Branch; T /= Tag ->
            false;
        [{B, T}|_] when B == Branch, T == Tag ->
            true;
        [] ->
            true;
        _Other ->
            false
    end,
    validate_bucket(O, Deps, Res and Prev).

arm_bucket(_Buckets, _Deps, _Current, _BuildPath, []) ->
    {ok, done};
arm_bucket(BucketsDets, Deps, Current, BuildPath, [Dep|O]) ->
    {BName, _, _} = Current,
    {Name, _, _} = Dep,
    ?LOCK(BName),
    [{BName, BPath, BPackages}] = dets:lookup(BucketsDets, BName),
    ?UNLOCK(BName),
    ?LOCK(Dep),
    [{Dep, {_, DepBuckets}, _, _}|_] = dets:lookup(Deps, Dep),
    ?UNLOCK(Dep),
    case lists:member(BName, DepBuckets) of
        true ->
            pass;
        false ->
            case [X || X <- DepBuckets, X /= BName] of
                [AnyBucket|_] ->
                    ?LOCK(AnyBucket),
                    [{AnyBucket, Path, _Packages}] = dets:lookup(BucketsDets, AnyBucket),
                    ?UNLOCK(AnyBucket),
                    DepPath = filename:join([BuildPath, Path, binary_to_list(Name)]),
                    copy_package_to_bucket(DepPath, filename:join([BuildPath, BPath, binary_to_list(Name)])),
                    ?LOCK(Dep),
                    [{Dep, {NewState, NewDepBuckets}, NewDepOn, NewHasInDep}|_] = dets:lookup(Deps, Dep),
                    ok = dets:insert(Deps, {Dep, {NewState, lists:usort([BName|NewDepBuckets])}, NewDepOn, NewHasInDep}),
                    ok = dets:insert(BucketsDets, {BName, BPath, lists:usort([Dep|BPackages])}),
                    ?UNLOCK(Dep);
                [] ->
                    DepPath = get_path(BuildPath, Dep, <<"new">>),
                    copy_package_to_bucket(DepPath, filename:join([BuildPath, BPath, binary_to_list(Name)])),
                    ?LOCK(Dep),
                    [{Dep, {NewState, NewDepBuckets}, NewDepOn, NewHasInDep}|_] = dets:lookup(Deps, Dep),
                    ok = dets:insert(Deps, {Dep, {NewState, lists:usort([BName|NewDepBuckets])}, NewDepOn, NewHasInDep}),
                    ok = dets:insert(BucketsDets, {BName, BPath, lists:usort([Dep|BPackages])}),
                    ?UNLOCK(Dep),
                    ?LOCK(BName),
                    [{BName, BPath, OldContain}] = dets:lookup(BucketsDets, BName),
                    ok = dets:insert(BucketsDets, {BName, BPath, lists:usort([Dep|OldContain])}),
                    ?UNLOCK(BName)
            end
    end,
    arm_bucket(BucketsDets, Deps, {BName, BPath, [Dep|BPackages]}, BuildPath, O).

copy_package_to_bucket(Source, Path) ->
    error_logger:info_msg("copying ~p to ~p~n", [Source, Path]),
    ?CU:del_dir(Path),
    filelib:ensure_dir(Path),
    ?CU:recursive_copy(Source, Path).

update_dep_buckets(BucketsTable, DepsTable, Buckets, BuildPath, Source, Rev) ->
    Package = ?VERSION(Rev),
    case catch update_buckets(BucketsTable, BuildPath, Source, Rev, Buckets) of
        {ok, UpdatedBuckets} ->
            SuccessB = [X || {X, _, _} <- UpdatedBuckets],
            ?LOCK(Package),
            [{Package, {NewState, NewBucketList}, NewDepObj, NewDepSubj}|_] = dets:lookup(DepsTable, Package),
            ok = dets:insert(DepsTable, 
                {Package, {NewState, lists:usort(SuccessB ++ NewBucketList)}, NewDepObj, NewDepSubj}),
            ?UNLOCK(Package),
            {ok, UpdatedBuckets};
        Other ->
            error_logger:error_msg("failed to update package buckets for ~p: ~p", [Package, Other]),
            throw(Other)
    end.

update_buckets(BucketsDB, Path, Source, Rev, Buckets) ->
    update_buckets(BucketsDB, Path, Source, Rev, Buckets, []).

update_buckets(_, _, _, _, [], Acc) ->
    {ok, Acc};
update_buckets(BucketsTable, BuildPath, Source, Rev, [Bucket|O], Acc) ->
    Package = ?VERSION(Rev),
    {BName, TempPath, TempContain} = Bucket,
    Deps = [{N, B, T} || {{N, B, T}, _S} <-Rev#rev_def.dep_object],
    ?LOCK(BName),
    [{BName, BPath, BContain}] = case dets:lookup(BucketsTable, BName) of
        [] -> 
            [{BName, TempPath, TempContain}];
        [{B, P, C}] ->
            [{B, P, C}]
    end,
    {Name, _B, _T} = Package,
    Path = filename:join([BuildPath, BPath, binary_to_list(Name)]) ++ "/",
    NewContain = lists:usort([Package|BContain] ++ Deps),
    try
        copy_package_to_bucket(Source, Path),
        ok = dets:insert(BucketsTable, {BName, BPath, lists:usort(NewContain)}),
        ?UNLOCK(BName)
    catch
        _:Reason ->
            ?UNLOCK(BName),
            error_logger:error_msg("failed to update bucket ~p: ~p", [BName, Reason]),
            throw(Reason)
    end,
    update_buckets(BucketsTable, BuildPath, Source, Rev, O, [{BName, BPath, NewContain}|Acc]).

delete_from_bucket(BucketsDB, Path, Bucket, Rev) ->
    Version = ?VERSION(Rev),
    ?LOCK(Bucket),
    [{BName, BPath, BContain}] = dets:lookup(BucketsDB, Bucket), 
    dets:insert(BucketsDB, {BName, BPath, lists:delete(Version, BContain)}),
    ?UNLOCK(Bucket),
    ToClean = filename:join([Path, BPath, binary_to_list(Rev#rev_def.name)]),
    error_logger:info_msg("cleaning up: ~p~n", [ToClean]),
    ?CU:del_dir(ToClean).

empty_state_buckets(DepsDB, Rev) ->
    Vsn = ?VERSION(Rev),
    ?LOCK(Vsn),
    [{Vsn, {State, _}, DepOn, HasInDep}|_] = dets:lookup(DepsDB, Vsn),
    ok = dets:insert(DepsDB, {Vsn, {State, []}, DepOn, HasInDep}),
    ?UNLOCK(Vsn),
    ok.

list_buckets(DepsDB, Rev) ->
    Version = ?VERSION(Rev),
    ?LOCK(Version),
    [{Version, {_, InBuckets}, _, _}] = dets:lookup(DepsDB, Version),
    ?UNLOCK(Version),
    InBuckets.

fetch_bucket(BucketsDB, BName) ->
    ?LOCK(BName),
    [Res|_] = dets:lookup(BucketsDB, BName), 
    ?UNLOCK(BName),
    Res.

cleanup_new_in_progress(Deps) ->
    lists:map(fun(X) -> update_new_in_progress(Deps, X) end, dets:match(Deps, '$1')).

update_new_in_progress(Deps, [{Vsn, {State, InB}, Subj, Obj}]) when State == <<"new">>; State == <<"in_progress">> ->
    dets:insert(Deps, {Vsn, {<<"none">>, InB}, Subj, Obj});
update_new_in_progress(Deps, _) ->
    ok.

-spec get_temp_path(list(), version()|#rev_def{}) -> list().
get_temp_path(Prefix, Rev) ->
    TmpName = filename:join([Prefix, "temp", get_dir_name(Rev, "tmp")]),
    filelib:ensure_dir(TmpName),
    TmpName.

-spec get_path(list(), version(), list()|binary()) -> list().
get_path(Prefix, Vsn, State) ->
    Dir = filename:join([Prefix, "pstore", get_dir_name(Vsn, State)]),
    filelib:ensure_dir(Dir ++ "/empty"),
    Dir.

-spec get_statpack_path(list(), integer(), version()) -> list().
get_statpack_path(Prefix, WorkId, RV) ->
    Dir = filename:join([Prefix, "buckets", get_dir_name(RV, integer_to_list(WorkId))]),
    filelib:ensure_dir(Dir),
    Dir.

get_dir_name(RV) ->
    get_dir_name(RV, <<"new">>).

get_dir_name(Rev=#rev_def{}, Postfix) ->
    Name = ?BTL(Rev#rev_def.name),
    Branch = ?BTL(Rev#rev_def.branch),
    Tag = ?BTL(Rev#rev_def.tag),
    lists:flatten(io_lib:format(
        "~s-~s-~s.~s", [Name, Branch, Tag, Postfix]));

get_dir_name({N, B, T}, Postfix) ->
    Name = ?BTL(N),
    Branch = ?BTL(B),
    Tag = ?BTL(T),
    lists:flatten(io_lib:format(
        "~s-~s-~s.~s", [Name, Branch, Tag, Postfix])).
