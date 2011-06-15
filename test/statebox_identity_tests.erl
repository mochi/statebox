-module(statebox_identity_tests).
-include_lib("eunit/include/eunit.hrl").

pid_cookie_identity_test() ->
    ?assertEqual(
       statebox_identity:pid_cookie(),
       statebox_identity:pid_cookie()).

pid_cookie_idempotent_test() ->
    NP = {Node, Pid} = {'node', erlang:list_to_pid("<0.0.0>")},
    ?assertEqual(
       erlang:phash2(NP),
       statebox_identity:pid_cookie(Node, Pid)).
