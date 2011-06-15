-module(statebox_counter_tests).
-behaviour(proper_statem).
-export([initial_state/0, command/1,
	 precondition/2, postcondition/3, next_state/3]).
-export([apply_f_inc_compact/2]).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

clock_step() ->
    1000.

default_age() ->
    10 * clock_step().

apply_f_inc_compact(Value, Counter) ->
    statebox:apply_op(
      statebox_counter:f_inc_compact(Value, default_age()),
      Counter).

%% statem
-record(state, {counter=[], value=[0]}).

initial_state() ->
    #state{}.

command(#state{counter=Counter}) ->
    oneof([{call, ?MODULE, apply_f_inc_compact, [range(-3, 3), Counter]},
           {call, statebox_counter, value, [Counter]}]).

precondition(_S, _Call) ->
    true.

postcondition(S, {call, _, value, _}, Res) ->
    lists:sum(S#state.value) =:= Res;
postcondition(S, {call, _, apply_f_inc_compact, [Inc, _]}, Res) ->
    (Inc + lists:sum(S#state.value)) =:= statebox_counter:value(Res).

next_state(S, _V, {call, _, value, _}) ->
    S;
next_state(S, V, {call, _, apply_f_inc_compact, [Inc, _C]}) ->
    S#state{counter=V, value=[Inc | S#state.value]}.

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
