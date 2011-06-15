-module(statebox_counter_tests).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

initial_test() ->
    ?assertEqual(
       0,
       statebox_counter:value([])),
    ok.

setup_statebox_clock_mock() ->
    M = statebox_clock,
    Inc = 1000,
    meck:new(M),
    T = ets:new(M, [public]),
    meck:expect(statebox_clock, timestamp,
                fun () -> try ets:update_counter(T, M, Inc)
                          catch error:badarg ->
                                  try ets:insert_new(T, M, 0)
                                  catch _:_ -> ets:update_counter(T, M, Inc)
                                  end
                          end
                end),
    {T, M}.

clock_generator(Suite) ->
    {foreach,
     fun setup_statebox_clock_mock/0,
     fun ({T, M}) -> meck:unload(M), catch ets:delete(T) end,
     Suite}.

proper_test_() ->
    clock_generator(
      [{atom_to_list(F),
        fun () -> ?assert(proper:quickcheck(?MODULE:F(), [long_result])) end}
       || {F, 0} <- ?MODULE:module_info(exports), F > 'prop_', F < 'prop`']).
