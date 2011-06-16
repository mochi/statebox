%% @doc Integer counter based on an ordered list of events.
-module(statebox_counter).
-export([value/1, merge/1, accumulate/2, inc/3]).
-export([f_inc_acc/2, f_inc_acc/3, op_inc_acc/4]).

-type op() :: statebox:op().
-type timestamp() :: statebox_clock:timestamp().
-type timedelta() :: statebox:timedelta().
-type counter_id() :: statebox_identity:entropy() | acc.
-type counter_key() :: {timestamp(), counter_id()}.
-type counter_op() :: {counter_key(), integer()}.
-type counter() :: [counter_op()].

-spec value(counter()) -> integer().
value([]) ->
    0;
value([{_Key, Value} | Rest]) ->
    Value + value(Rest).

-spec merge([counter()]) -> counter().
merge([Counter]) ->
    Counter;
merge(Counters) ->
    orddict:from_list(merge_prune(Counters)).

-spec accumulate(timestamp(), counter()) -> counter().
accumulate(Timestamp, Counter=[{{T0, acc}, _} | _]) when Timestamp =< T0 ->
    Counter;
accumulate(Timestamp, Counter) ->
    accumulate(Timestamp, Counter, 0).

-spec inc(counter_key(), integer(), counter()) -> counter().
inc({T1, _Id1}, _Value, Counter=[{{T0, acc}, _} | _Rest]) when T1 =< T0 ->
    Counter;
inc(Key, Value, Counter) ->
    orddict:store(Key, Value, Counter).

-spec f_inc_acc(integer(), timedelta()) -> op().
f_inc_acc(Value, Age) ->
    Key = {statebox_clock:timestamp(), statebox_identity:entropy()},
    f_inc_acc(Value, Age, Key).

-spec f_inc_acc(integer(), timedelta(), counter_key()) -> op().
f_inc_acc(Value, Age, Key={Timestamp, _Id}) ->
    {fun ?MODULE:op_inc_acc/4, [Timestamp - Age, Key, Value]}.

%% @private
op_inc_acc(Timestamp, Key, Value, Counter) ->
    accumulate(Timestamp, inc(Key, Value, Counter)).

%% Internal API

merge_prune(Counters) ->
    prune(lists:umerge(Counters)).

prune(All) ->
    prune(All, All).

prune(Here=[{{_Ts, acc}, _V} | Rest], _Last) ->
    prune(Rest, Here);
prune([_ | Rest], Last) ->
    prune(Rest, Last);
prune([], Last) ->
    Last.

accumulate(Timestamp, [{{T1, _Id}, Value} | Rest], Sum) when T1 =< Timestamp ->
    accumulate(Timestamp, Rest, Value + Sum);
accumulate(Timestamp, Counter, Sum) ->
    inc({Timestamp, acc}, Sum, Counter).
