%% @doc A "monad" for wrapping a value with a ordered event queue
%%      such that values that have diverged in history can be merged
%%      automatically in a predictable manner.
%%
%%      In order to provide for an efficient serialization, old events
%%      can be expired with expire/2 and the event queue can be
%%      truncated to a specific maximum size with truncate/2.
%%
%%      The default representation for a timestamp is OS clock msecs,
%%      defined by <code>statebox_clock:timestamp/0</code>. This is
%%      used by the convenience functions <code>new/1</code> and
%%      <code>modify/2</code>.
-module(statebox).
-export([new/2, modify/3, merge/1, expire/2, truncate/2,
         new/1, modify/2,
         value/1, last_modified/1]).

-record(statebox, {
          value :: term(),
          %% sorted list of operations (oldest first).
          queue :: [event()],
          last_modified :: timestamp()}).
-opaque statebox() :: #statebox{}.
-type event() :: {timestamp(), op()}.
-type timestamp() :: integer().
-type timedelta() :: integer().
-type op() :: {module(), atom(), [term()]} |
              {fun((...) -> statebox()), [term()]}.

%% Used in a test, must be done before function definitions.
-ifdef(TEST).
-export([dummy_mfa_4/4]).
-endif.

%% @doc Construct a statebox at <code>statebox_clock:timestamp()</code>
%%      containing the result of <code>Constructor()</code>. This should
%%      return an "empty" object of the desired type, such as
%%      <code>fun gb_trees:empty/0</code>.
%% @equiv new(timestamp(), Constructor)
new(Constructor) ->
    new(statebox_clock:timestamp(), Constructor).

%% @doc Construct a statebox containing the result of
%%      <code>Constructor()</code>. This should return an "empty" object of
%%      the desired type, such as <code>fun gb_trees:empty/0</code>.
-spec new(timestamp(), fun(() -> term())) -> statebox().
new(T, Constructor) ->
    new(T, Constructor(), []).

%% @doc Return the current value of the statebox. You should consider this
%%      value to be read-only.
-spec value(statebox()) -> term().
value(#statebox{value=V}) ->
    V.

%% @doc Return the last modified timestamp of the statebox.
-spec last_modified(statebox()) -> timestamp().
last_modified(#statebox{last_modified=T}) ->
    T.

%% @doc Remove all events older than <code>last_modified(S) - Age</code>
%%      from the event queue.
-spec expire(timedelta(), statebox()) -> statebox().
expire(Age, State=#statebox{queue=Q, last_modified=T}) ->
    OldT = T - Age,
    State#statebox{
      queue=lists:dropwhile(fun ({EventT, _}) -> EventT < OldT end, Q)}.

%% @doc Truncate the event queue to the newest N events.
-spec truncate(non_neg_integer(), statebox()) -> statebox().
truncate(N, State=#statebox{queue=Q}) ->
    case length(Q) - N of
        Tail when Tail > 0 ->
            State#statebox{queue=lists:nthtail(Tail, Q)};
        _ ->
            State
    end.

%% @doc Return a new statebox as the product of all in-order events applied to
%%      the last modified statebox(). If two events occur at the same time, the
%%      event that sorts lowest by value will be applied first.
-spec merge([statebox()]) -> statebox().
merge([State]) ->
    State;
merge(Unordered) ->
    #statebox{value=V, last_modified=T} = newest(Unordered),
    Queue = lists:umerge([Q || #statebox{queue=Q} <- Unordered]),
    new(T, apply_queue(V, Queue), Queue).

%% @doc Modify the value in statebox and add {T, Op} to its event queue.
%%      Op should be a <code>{M, F, Args}</code> or <code>{Fun, Args}</code>.
%%      The value will be transformed as such:
%%      <code>NewValue = apply(Fun, Args ++ [value(S)])</code>.
%%      The operation should be repeatable and should return the same type as
%%      <code>value(S)</code>. This means that this should hold true:
%%      <code>Fun(Arg, S) =:= Fun(Arg, Fun(Arg, S))</code>.
%%      An example of this kind of operation is <code>orddict:store/3</code>.
%%      Only exported operations should be used in order to ensure that the
%%      serialization is small and robust (this is not enforced).
-spec modify(timestamp(), op(), statebox()) -> statebox().
modify(T, Op, #statebox{value=Value, queue=Queue, last_modified=OldT})
  when OldT =< T ->
    Event = {T, Op},
    new(T, apply_event(Event, Value), queue_in(Event, Queue));
modify(T, _Op, #statebox{last_modified=OldT}) ->
    throw({invalid_timestamp, {T, '<', OldT}}).

%% @doc Modify a statebox at timestamp
%%      <code>max(1 + last_modified(S), statebox_clock:timestamp())</code>.
%%      See <code>modify/3</code> for more information.
%% @equiv modify(max(1 + last_modified(S), statebox_clock:timestamp()), Op, S)
-spec modify(op(), statebox()) -> statebox().
modify(Op, S) ->
    modify(max(1 + last_modified(S), statebox_clock:timestamp()), Op, S).

%% Internal API

newest([First | Rest]) ->
    newest(First, Rest).

newest(M0, [M1 | Rest]) ->
    case last_modified(M0) >= last_modified(M1) of
        true ->
            newest(M0, Rest);
        false ->
            newest(M1, Rest)
    end;
newest(M, []) ->
    M.

new(T, V, Q) ->
    #statebox{value=V, queue=Q, last_modified=T}.

queue_in(Event, Queue) ->
    Queue ++ [Event].

apply_queue(Data, Queue) ->
    lists:foldl(fun apply_event/2, Data, Queue).

apply_event({_T, {F, [A]}}, Data) when is_function(F, 2) ->
    F(A, Data);
apply_event({_T, {F, [A, B]}}, Data) when is_function(F, 3) ->
    F(A, B, Data);
apply_event({_T, {F, A}}, Data) when is_function(F) ->
    apply(F, A ++ [Data]);
apply_event({_T, {M, F, [A]}}, Data) ->
    M:F(A, Data);
apply_event({_T, {M, F, [A, B]}}, Data) ->
    M:F(A, B, Data);
apply_event({_T, {M, F, A}}, Data) ->
    apply(M, F, A ++ [Data]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
new_test() ->
    Now = 1,
    S = new(Now, fun () -> data end),
    ?assertEqual(
       data,
       value(S)),
    ?assertEqual(
       Now,
       last_modified(S)),
    %% Nothing to expire
    ?assertEqual(
       S,
       expire(0, S)),
    %% Nothing to truncate
    ?assertEqual(
       S,
       truncate(16, S)),
    %% Nothing to merge
    ?assertEqual(
       S,
       merge([S])),
    %% Merging the same object
    ?assertEqual(
       S,
       merge([S, S])),
    ok.

bad_modify_test() ->
    F = fun (N, S) -> modify(N, {fun ordsets:add_element/2, [N]}, S) end,
    S10 = lists:foldl(F, new(0, fun () -> [] end), lists:seq(1, 10)),
    ?assertEqual(
       lists:seq(1, 10),
       value(S10)),
    ?assertThrow(
       {invalid_timestamp, {9, '<', 10}},
       F(9, S10)),
    ok.

%% @private
dummy_mfa_4(a, b, C, D) ->
    ordsets:add_element(C, D).

alt_apply_op_test() ->
    L = [fun (N=1) -> {ordsets, add_element, [N]} end,
         fun (N=2) ->
                 {fun (a, B, C) -> ordsets:add_element(B, C) end, [a, N]}
         end,
         fun (N=3) ->
                 {fun ?MODULE:dummy_mfa_4/4, [a, b, N]}
         end,
         fun (N=4) ->
                 {?MODULE, dummy_mfa_4, [a, b, N]}
         end,
         fun (N=5) ->
                 {ordsets, fold,
                  [fun (X, Acc) -> ordsets:add_element(X + N, Acc) end, []]}
         end],
    F = fun ({N, F}, S) -> modify(N, F(N), S) end,
    S5 = lists:foldl(F, new(0, fun () -> [] end),
                     lists:zip(lists:seq(1, 5), L)),
    ?assertEqual(
       lists:seq(5 + 1, 5 + 4),
       value(S5)),
    ok.

truncate_test() ->
    F = fun (N, S) -> modify(N, {fun ordsets:add_element/2, [N]}, S) end,
    S10 = lists:foldl(F, new(0, fun () -> [] end), lists:seq(1, 10)),
    ?assertEqual(
       lists:seq(1, 10),
       value(S10)),
    ?assertEqual(
       10,
       length(S10#statebox.queue)),
    ?assertEqual(
       10,
       length((truncate(20, S10))#statebox.queue)),
    ?assertEqual(
       10,
       length((truncate(10, S10))#statebox.queue)),
    ?assertEqual(
       1,
       length((truncate(1, S10))#statebox.queue)),
    ok.

expire_test() ->
    F = fun (N, S) -> modify(N, {fun ordsets:add_element/2, [N]}, S) end,
    S10 = lists:foldl(F, new(0, fun () -> [] end), lists:seq(1, 10)),
    ?assertEqual(
       lists:seq(1, 10),
       value(S10)),
    ?assertEqual(
       10,
       length(S10#statebox.queue)),
    ?assertEqual(
       1,
       length((expire(0, S10))#statebox.queue)),
    ?assertEqual(
       10,
       length((expire(10, S10))#statebox.queue)),
    ?assertEqual(
       10,
       length((expire(11, S10))#statebox.queue)),
    ok.

orddict_in_a_statebox_test() ->
    S0 = new(0, fun () -> [] end),
    ?assertEqual(
       [],
       value(S0)),
    S1_a = modify(1, {fun orddict:store/3, [key, a]}, S0),
    S1_b = modify(1, {fun orddict:store/3, [key, b]}, S0),
    S1_c = modify(1, {fun orddict:store/3, [c, c]}, S0),
    S2_aa = modify(3, {fun orddict:store/3, [key, a2]}, S1_a),
    S2_ab = modify(2, {fun orddict:store/3, [key, b2]}, S1_a),
    S2_bb = modify(2, {fun orddict:store/3, [key, b2]}, S1_b),
    ?assertEqual(
       1,
       last_modified(S1_a)),
    ?assertEqual(
       1,
       last_modified(S1_b)),
    ?assertEqual(
       [{key, a}],
       value(S1_a)),
    ?assertEqual(
       [{key, b}],
       value(S1_b)),
    ?assertEqual(
       S1_a,
       merge([S1_a])),
    ?assertEqual(
       S1_a,
       merge([S0, S1_a])),
    ?assertEqual(
       S1_a,
       merge([S1_a, S0])),
    %% This is a conflict that can not be resolved peacefully,
    %% but S1_b wins by op compare
    ?assertEqual(
       value(S1_b),
       value(merge([S1_a, S1_b]))),
    %% This is a conflict that can not be resolved peacefully,
    %% but S1_b wins by op compare
    ?assertEqual(
       value(S1_b),
       value(merge([S1_b, S1_a]))),
    %% S2_aa wins because it has a bigger timestamp
    ?assertEqual(
       value(S2_aa),
       value(merge([S2_aa, S2_ab]))),
    %% S2_aa wins because it has a bigger timestamp
    ?assertEqual(
       value(S2_aa),
       value(merge([S2_ab, S2_aa]))),
    %% S2_aa wins because it has a bigger timestamp
    ?assertEqual(
       value(S2_aa),
       value(merge([S2_bb, S2_aa]))),
    %% S2_aa wins because it has a bigger timestamp
    ?assertEqual(
       value(S2_aa),
       value(merge([S2_aa, S2_bb]))),
    %% S1_[ab] and S1_c collide in time but the operations do not conflict
    ?assertEqual(
       [{c, c}, {key, a}],
       value(merge([S1_a, S1_c]))),
    ?assertEqual(
       [{c, c}, {key, a}],
       value(merge([S1_c, S1_a]))),
    ?assertEqual(
       [{c, c}, {key, b}],
       value(merge([S1_b, S1_c]))),
    ?assertEqual(
       [{c, c}, {key, b}],
       value(merge([S1_c, S1_b]))),
    %% S1_b wins over S1_a by op compare but S1_c is independent
    ?assertEqual(
       [{c, c}, {key, b}],
       value(merge([S1_c, S1_a, S1_b]))),
    ?assertEqual(
       [{c, c}, {key, b}],
       value(merge([S1_c, S1_b, S1_a]))),
    ?assertEqual(
       [{c, c}, {key, b}],
       value(merge([S1_a, S1_b, S1_c]))),
    ?assertEqual(
       [{c, c}, {key, b}],
       value(merge([S1_a, S1_c, S1_b]))),
    ok.

-define(WHENEVER, 1303513575954).
convenience_test_() ->
    {setup,
     fun () ->
             meck:new(statebox_clock),
             meck:expect(statebox_clock, timestamp, 0, ?WHENEVER)
     end,
     fun (_) -> meck:unload(statebox_clock) end,
     [{"new",
       fun () ->
               ?assertEqual(
                  ?WHENEVER,
                  last_modified(new(fun () -> [] end)))
       end},
      {"modify",
       fun () ->
               S = modify({fun ordsets:add_element/2, [a]},
                          new(0, fun () -> [] end)),
               S1 = modify({fun ordsets:add_element/2, [b]},
                          S),
               ?assertEqual(
                  ?WHENEVER,
                  last_modified(S)),
               ?assertEqual(
                  [a],
                  value(S)),
               %% Check for clock skew correction
               ?assertEqual(
                  1 + ?WHENEVER,
                  last_modified(S1)),
               ?assertEqual(
                  [a, b],
                  value(S1))
       end}]}.

-endif.
