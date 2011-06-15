%% @doc Functions for uniquely identifying Erlang processes.
-module(statebox_identity).

-export([pid_cookie/2, pid_cookie/0]).

-type pid_cookie() :: 1..4294967296.

%% @equiv pid_cookie(node(), self()).
-spec pid_cookie() -> pid_cookie().
pid_cookie() ->
    pid_cookie(node(), self()).

%% @doc Return an integer that uniquely represents the node and pid
%%      combination at this moment. Should be combined with a timestamp
%%      if used as a globally unique identifier.
-spec pid_cookie(node(), pid()) -> pid_cookie().
pid_cookie(Node, Pid) ->
    erlang:phash2({Node, Pid}).
