%% @doc Statebox wrappers for orddict-based storage.
-module(statebox_orddict).
-export([from_values/1, orddict_from_proplist/1]).
-export([is_empty/1]).
-export([f_union/2, f_subtract/2, f_update/2, f_merge/1,
         f_merge_proplist/1, f_store/2, f_delete/1, f_erase/1]).
-export([op_union/3, op_subtract/3, op_update/3, op_merge/2]).

%% External API

-type proplist() :: [{term(), term()}].
-type orddict() :: proplist().
-type statebox() :: statebox:statebox().
-type op() :: statebox:op().

%% @doc Return a statebox() from a list of statebox() and/or proplist().
%%      proplist() are convered to a new statebox() with f_merge_proplist/2
%%      before merge.
-spec from_values([proplist() | statebox()]) -> statebox().
from_values([]) ->
    statebox:new(fun () -> [] end);
from_values(Vals) ->
    statebox:merge([as_statebox(V) || V <- Vals]).

%% @doc Convert a proplist() to an orddict() (by sorting it).
%%      Only [{term(), term()}] proplists are supported.
-spec orddict_from_proplist(proplist()) -> orddict().
orddict_from_proplist(P) ->
    lists:usort(P).

%% @doc Return true if the statebox's value is [], false otherwise.
-spec is_empty(statebox()) -> boolean().
is_empty(Box) ->
    statebox:value(Box) =:= [].

%% @doc Returns an op() that does an ordsets:union(New, Set) on the value at
%%      K in orddict (or [] if not present).
-spec f_union(term(), [term()]) -> op().
f_union(K, New) ->
    {fun ?MODULE:op_union/3, [K, New]}.

%% @doc Returns an op() that does an ordsets:union(Del, Set) on the value at
%%      K in orddict (or [] if not present).
-spec f_subtract(term(), [term()]) -> op().
f_subtract(K, Del) ->
    {fun ?MODULE:op_subtract/3, [K, Del]}.

%% @doc Returns an op() that merges the proplist New to the orddict.
-spec f_merge_proplist(term()) -> op().
f_merge_proplist(New) ->
    f_merge(orddict_from_proplist(New)).

%% @doc Returns an op() that merges the orddict New to the orddict.
-spec f_merge(term()) -> op().
f_merge(New) ->
    {fun ?MODULE:op_merge/2, [New]}.

%% @doc Returns an op() that updates the value at Key in orddict (or [] if
%%      not present) with the given Op.
-spec f_update(term(), op()) -> op().
f_update(Key, Op) ->
    {fun ?MODULE:op_update/3, [Key, Op]}.

%% @doc Returns an op() that stores Value at Key in orddict.
-spec f_store(term(), term()) -> op().
f_store(Key, Value) ->
    {fun orddict:store/3, [Key, Value]}.

%% @doc Returns an op() that deletes the pair if present from orddict.
-spec f_delete({term(), term()}) -> op().
f_delete(Pair={_, _}) ->
    {fun lists:delete/2, [Pair]}.

%% @doc Returns an op() that erases the value at K in orddict.
-spec f_erase(term()) -> op().
f_erase(Key) ->
    {fun orddict:erase/2, [Key]}.

%% Statebox ops

%% @private
op_union(K, New, D) ->
    orddict:update(K, fun (Old) -> ordsets:union(Old, New) end, New, D).

%% @private
op_subtract(K, Del, D) ->
    orddict:update(K, fun (Old) -> ordsets:subtract(Old, Del) end, [], D).

%% @private
op_merge(New, D) ->
    orddict:merge(fun (_Key, _OldV, NewV) -> NewV end, D, New).

%% @private
op_update(Key, Op, [{K, _}=E | Dict]) when Key < K ->
    %% This is very similar to orddict:update/4.
    [{Key, statebox:apply_op(Op, [])}, E | Dict];
op_update(Key, Op, [{K, _}=E | Dict]) when Key > K ->
    [E | op_update(Key, Op, Dict)];
op_update(Key, Op, [{_K, Val} | Dict]) -> %% Key == K
    [{Key, statebox:apply_op(Op, Val)} | Dict];
op_update(Key, Op, []) ->
    [{Key, statebox:apply_op(Op, [])}].

%% Internal API

as_statebox(V) ->
    case statebox:is_statebox(V) of
        true ->
            V;
        false ->
            from_legacy(V)
    end.

from_legacy(D) ->
    %% Legacy objects should always be overwritten.
    statebox:modify(f_merge_proplist(D), statebox:new(fun () -> [] end)).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
orddict_from_proplist_test() ->
    ?assertEqual(
       [{a, b}, {c, d}],
       orddict_from_proplist([{c, d}, {a, b}])),
    ok.

is_empty_test() ->
    ?assertEqual(
       true,
       is_empty(statebox:new(fun () -> [] end))),
    ?assertEqual(
       false,
       is_empty(
         statebox:modify(f_store(foo, bar),
                         statebox:new(fun () -> [] end)))),
    ok.

f_subtract_test() ->
    ?assertEqual(
       [{taco, [a, b]}],
       statebox:apply_op(f_subtract(taco, [c, d]), [{taco, [a, b, c]}])),
    ok.

f_merge_proplist_test() ->
    ?assertEqual(
       [{a, b}, {c, d}, {e, f}],
       statebox:apply_op(f_merge_proplist([{e, f}, {c, d}]), [{a, b}])),
    ok.

f_merge_test() ->
    ?assertEqual(
       [{a, b}, {c, d}, {e, f}],
       statebox:apply_op(f_merge([{a, b}, {c, d}, {e, f}]), [{a, a}])),
    ok.

f_update_test() ->
    ?assertEqual(
       [{b, [{b, c}]}],
       statebox:apply_op(f_update(b, f_store(b, c)), [])),
    ?assertEqual(
       [{b, [{b, c}]}, {c, c}],
       statebox:apply_op(f_update(b, f_store(b, c)), [{c, c}])),
    ?assertEqual(
       [{a, a}, {b, [{a, a}, {b, c}]}],
       statebox:apply_op(f_update(b, f_store(b, c)), [{a, a}, {b, [{a, a}]}])),
    ok.

f_store_test() ->
    ?assertEqual(
       [{a, b}],
       statebox:apply_op(f_store(a, b), [])),
    ok.

f_delete_test() ->
    ?assertEqual(
       [{a, b}],
       statebox:apply_op(f_delete({a, a}), [{a, b}])),
    ?assertEqual(
       [],
       statebox:apply_op(f_delete({a, b}), [{a, b}])),
    ok.

f_erase_test() ->
    ?assertEqual(
       [{a, b}],
       statebox:apply_op(f_erase(b), [{a, b}])),
    ?assertEqual(
       [],
       statebox:apply_op(f_erase(a), [{a, b}])),
    ok.

f_union_test() ->
    ?assertEqual(
       [{taco, [a, b, c]}],
       statebox:apply_op(f_union(taco, [a, b]), [{taco, [c]}])),
    ?assertEqual(
       [{taco, [a, b]}],
       statebox:apply_op(f_union(taco, [a, b]), [])),
    ok.

from_values_test() ->
    ?assertEqual(
       [],
       statebox:value(from_values([]))),
    ?assertEqual(
       [],
       statebox:value(from_values([[]]))),
    ?assertEqual(
       [{<<"binary">>, value}],
       statebox:value(from_values([[{<<"binary">>, value}]]))),
    ?assertEqual(
       [{atom, value}],
       statebox:value(from_values([[{atom, value}]]))),
    ?assertEqual(
       [{services, [{key, [1, 2]}]}, {x, y}],
       statebox:value(from_values([[{x, y}, {services, [{key, [1, 2]}]}]]))),
    ?assertEqual(
       [],
       statebox:value(from_values([statebox:new(fun () -> [] end)]))),
    ?assertEqual(
       [{key, value}],
       statebox:value(from_values([from_values([[{key, value}]])]))),
    ok.

readme_orddict_test() ->
    New = statebox_orddict:from_values([]),
    ChildA = statebox:modify([statebox_orddict:f_store(a, 1),
                              statebox_orddict:f_union(c, [a, aa])],
                             New),
    ChildB = statebox:modify([statebox_orddict:f_store(b, 1),
                              statebox_orddict:f_union(c, [b, bb])],
                             New),
    Resolved = statebox_orddict:from_values([ChildA, ChildB]),
    ?assertEqual(
       [{a, 1}, {b, 1}, {c, [a, aa, b, bb]}],
       statebox:value(Resolved)),
    ok.

-endif.
