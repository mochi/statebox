%% @doc Functions for uniquely identifying events.
-module(statebox_identity).

-export([entropy/2, entropy/0]).

-type entropy() :: 1..4294967296.
-export_type([entropy/0]).

%% @equiv entropy(node(), statebox_clock:now())
-spec entropy() -> entropy().
entropy() ->
    entropy(node(), statebox_clock:now()).

%% @doc Return an integer that can be expected to be reasonably unique
%%      at a given msec timestamp.
-spec entropy(node(), statebox_clock:t_now()) -> entropy().
entropy(Node, Now) ->
    erlang:phash2({Node, Now}).
