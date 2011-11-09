-module(statebox_identity_tests).
-include_lib("eunit/include/eunit.hrl").

entropy_unique_test() ->
    ?assertNot(
       statebox_identity:entropy() =:= statebox_identity:entropy()).

entropy_idempotent_test() ->
    NP = {Node, Now} = {'node', {1, 2, 3}},
    ?assertEqual(
       erlang:phash2(NP),
       statebox_identity:entropy(Node, Now)),
    ?assertEqual(
       statebox_identity:entropy(Node, Now),
       statebox_identity:entropy(Node, Now)).
