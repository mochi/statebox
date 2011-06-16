-module(statebox_counter_tests).
-behaviour(proper_statem).
-export([initial_state/0, command/1,
	 precondition/2, postcondition/3, next_state/3]).
-export([apply_f_inc_acc/4, add_sibling/0, merge_siblings/1]).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

clock_step() ->
    1000.

default_age() ->
    10 * clock_step().

apply_f_inc_acc(Value, Clock, N, Counters) ->
    Key = {Clock, statebox_identity:entropy()},
    statebox:apply_op(
      statebox_counter:f_inc_acc(Value, default_age(), Key),
      lists:nth(N, Counters)).

add_sibling() ->
    ok.

merge_siblings(_N) ->
    ok.

%% statem

%% TODO:
%% Generate a new sibling   (add_sibling)
%% Update existing counter  (apply_f_inc_acc)
%% Merge (up to) N siblings (merge_siblings)
%% Expiration used will be for 20 clock cycles
-record(state, {counters=[[]], num_counters=1, value=[], clock=0}).

initial_state() ->
    #state{clock=10000}.

command(#state{counters=Counters, num_counters=N, clock=Clock}) ->
    oneof([{call, ?MODULE, add_sibling, []},
           {call, ?MODULE, merge_siblings, [range(1, N)]},
           {call, ?MODULE, apply_f_inc_acc, [range(-3, 3), Clock, range(1, N), Counters]}]).

precondition(_S, _Call) ->
    true.

postcondition(S, {call, _, apply_f_inc_acc, [Inc, _]}, Res) ->
    sane_counter(Res)
        andalso (Inc + lists:sum(S#state.value)) =:= statebox_counter:value(Res);
postcondition(S, {call, _, _, _}, _Res) ->
    lists:all(fun sane_counter/1, S#state.counters)
        andalso (lists:sum(S#state.value) =:=
                     statebox_counter:value(statebox_counter:merge(S#state.counters))).

next_state(S=#state{counters=[H|T]}, _V, {call, _, add_sibling, []}) ->
    S#state{counters=[H,H|T]};
next_state(S=#state{counters=Counters}, _V, {call, _, merge_siblings, [N]}) ->
    {L, T} = lists:split(N, Counters),
    S#state{counters=[statebox_counter:merge(L) | T]};
next_state(S=#state{counters=Counters, clock=Clock}, V, {call, _, apply_f_inc_acc, [Inc, Clock, N, _C]}) ->
    Counters1 = lists:sublist(Counters, N - 1) ++ [V | lists:nthtail(N, Counters)],
    S#state{counters=Counters1,
            value=[Inc | S#state.value],
            clock=Clock + clock_step()}.

sane_counter([]) ->
    true;
sane_counter([{Timestamp, Id} | Rest]) ->
    sane_counter(Rest, Id =:= acc, Timestamp).

sane_counter([A, A | _], _, _) ->
    false;
sane_counter([{T1, _} | _], true, T0) when T1 < T0 ->
    false;
sane_counter([{_, acc} | _], true, _) ->
    false;
sane_counter([{T1, acc} | Rest], false, _T0) ->
    sane_counter(Rest, true, T1);
sane_counter([{T1, _} | Rest], HasAcc, _T0) ->
    sane_counter(Rest, HasAcc, T1);
sane_counter([], _, _) ->
    true.

%% properties

prop_counter_works_fine() ->
    ?FORALL(Cmds, commands(?MODULE),
            ?TRAPEXIT(
                begin
                    {History,State,Result} = run_commands(?MODULE, Cmds),
                    ?WHENFAIL(io:format("History: ~w\nState: ~w\nResult: ~w\n",
                                        [History,State,Result]),
                              aggregate(command_names(Cmds), Result =:= ok))
                end)).

%% tests

initial_test() ->
    ?assertEqual(
       0,
       statebox_counter:value([])),
    ok.

inc_test() ->
    C0 = [],
    C1 = statebox_counter:inc({1, 1}, 1, C0),
    C2 = statebox_counter:inc({2, 2}, 1, C1),
    ?assertEqual(
       0,
       statebox_counter:value(C0)),
    ?assertEqual(
       1,
       statebox_counter:value(C1)),
    ?assertEqual(
       2,
       statebox_counter:value(C2)),
    ok.

merge_test() ->
    C0 = [],
    C1 = statebox_counter:inc({1, 1}, 1, C0),
    C2 = statebox_counter:inc({2, 2}, 1, C1),
    ?assertEqual(
       2,
       statebox_counter:value(statebox_counter:merge([C0, C1, C2]))),
    ?assertEqual(
       1,
       statebox_counter:value(statebox_counter:merge([C0, C1, C1]))),
    ?assertEqual(
       1,
       statebox_counter:value(statebox_counter:merge([C1]))),
    ?assertEqual(
       0,
       statebox_counter:value(statebox_counter:merge([C0, C0]))),
    ok.

next_clock(N) ->
    Next = N + clock_step(),
    meck:expect(statebox_clock, timestamp, fun () -> next_clock(Next) end),
    Next.

setup_clock() ->
    meck:new(statebox_clock, [passthrough]),
    next_clock(100000).

cleanup_clock(_) ->
    meck:unload(statebox_clock).

proper_test_() ->
    {foreach,
     fun setup_clock/0,
     fun cleanup_clock/1,
     [{atom_to_list(F),
       fun () -> ?assert(proper:quickcheck(?MODULE:F(), [long_result])) end}
      || {F, 0} <- ?MODULE:module_info(exports), F > 'prop_', F < 'prop`']}.
