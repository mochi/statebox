%% @doc Integer counter based on an ordered list of events.
-module(statebox_counter).
-export([value/1, merge/1, compact/2, inc/3, f_inc_compact/2, op_inc_compact/4]).

-type op() :: statebox:op().
-type timestamp() :: statebox_clock:timestamp().
-type timedelta() :: statebox:timedelta().
-type counter_id() :: statebox_identity:entropy() | merged.
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
    orddict:from_list(lists:umerge(compact_heads(Counters))).

-spec compact(timestamp(), counter()) -> counter().
compact(Timestamp, Counter) ->
    compact(Timestamp, Counter, 0).

-spec inc(counter_key(), integer(), counter()) -> counter().
inc(Key, Value, Counter) ->
    orddict:store(Key, Value, Counter).

-spec f_inc_compact(integer(), timedelta()) -> op().
f_inc_compact(Value, Age) ->
    Timestamp = statebox_clock:timestamp(),
    Key = {Timestamp, statebox_identity:entropy()},
    {fun ?MODULE:op_inc_compact/4, [Timestamp - Age, Key, Value]}.

%% @private
op_inc_compact(Timestamp, Key, Value, Counter) ->
    compact(Timestamp, inc(Key, Value, Counter)).

%% Internal API

compact(Timestamp, [{{T1, _Id}, Value} | Rest], Sum) when T1 =< Timestamp ->
    compact(Timestamp, Rest, Value + Sum);
compact(Timestamp, Counter, Sum) ->
    [{{Timestamp, merged}, Sum} | Counter].

compact_heads(Counters) ->
    Timestamp = max_head(Counters),
    [compact_head(Counter, Timestamp) || Counter <- Counters].

compact_head([{{T0, _Id}, _V} | Rest], Timestamp) when Timestamp > T0 ->
    compact_head(Rest, Timestamp);
compact_head(Counter, _Timestamp) ->
    Counter.

max_head(Counter) ->
    max_head(Counter, 0).

max_head([], Timestamp) ->
    Timestamp;
max_head([{{T0, merged}, _V} | Rest], Timestamp) when T0 > Timestamp ->
    max_head(Rest, T0);
max_head([_ | Rest], Timestamp) ->
    max_head(Rest, Timestamp).
