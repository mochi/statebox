%% @doc Integer counter based on an ordered list of counter events.
%%
%% A counter is stored as an orddict of counter events. Each counter
%% event has a unique key based on the timestamp and some entropy, and it
%% stores the delta from the inc operation. The value of a counter is the
%% sum of all these deltas.
%%
%% As an optimization, counter events older than a given age are coalesced
%% to a single counter event with a key in the form of
%% <code>{timestamp(), 'acc'}</code>.

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

%% @doc Return the value of the counter (the sum of all counter event deltas).
-spec value(counter()) -> integer().
value([]) ->
    0;
value([{_Key, Value} | Rest]) ->
    Value + value(Rest).

%% @doc Merge the given list of counters and return a new counter
%% with the union of that history.
-spec merge([counter()]) -> counter().
merge([Counter]) ->
    Counter;
merge(Counters) ->
    orddict:from_list(merge_prune(Counters)).

%% @doc Accumulate all counter events older than <code>Timestamp</code> to
%% the key <code>{Timestamp, acc}</code>. If there is already an
%% <code>acc</code> at or before <code>Timestamp</code> this is a no-op.
-spec accumulate(timestamp(), counter()) -> counter().
accumulate(Timestamp, Counter=[{{T0, acc}, _} | _]) when Timestamp =< T0 ->
    Counter;
accumulate(Timestamp, Counter) ->
    accumulate(Timestamp, Counter, 0).

%% @doc Return a new counter with the given counter event. If there is
%% an <code>acc</code> at or before the timestamp of the given key then
%% this is a no-op.
-spec inc(counter_key(), integer(), counter()) -> counter().
inc({T1, _Id1}, _Value, Counter=[{{T0, acc}, _} | _Rest]) when T1 =< T0 ->
    Counter;
inc(Key, Value, Counter) ->
    orddict:store(Key, Value, Counter).

%% @equiv f_inc_acc(Value, Age, {statebox_clock:timestamp(),
%%                               statebox_identity:entropy()})
-spec f_inc_acc(integer(), timedelta()) -> op().
f_inc_acc(Value, Age) ->
    Key = {statebox_clock:timestamp(), statebox_identity:entropy()},
    f_inc_acc(Value, Age, Key).

%% @doc Return a statebox event to increment and accumulate the counter.
%% <code>Value</code> is the delta,
%% <code>Age</code> is the maximum age of counter events in milliseconds
%% (this should be longer than the amount of time you expect your cluster to
%% reach a consistent state),
%% <code>Key</code> is the counter event key.
-spec f_inc_acc(integer(), timedelta(), counter_key()) -> op().
f_inc_acc(Value, Age, Key={Timestamp, _Id}) ->
    {fun ?MODULE:op_inc_acc/4, [Timestamp - Age, Key, Value]}.

%% @private
op_inc_acc(Timestamp, Key, Value, Counter) ->
    accumulate(Timestamp, inc(Key, Value, Counter)).

%% Internal API

merge_prune(Counters) ->
    %% Merge of all of the counters and prune all entries older than the
    %% newest {_, acc}.
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
    %% Roll up old counter events
    accumulate(Timestamp, Rest, Value + Sum);
accumulate(Timestamp, Counter, Sum) ->
    %% Return the new counter
    inc({Timestamp, acc}, Sum, Counter).
