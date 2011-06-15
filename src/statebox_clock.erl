%% @doc Timestamp functions for <code>statebox:new/1</code> and
%%      <code>statebox:modify/2</code>.

-module(statebox_clock).
-export([timestamp/0, now_to_msec/1, now/0]).

-define(KILO, 1000).
-define(MEGA, 1000000).

%% @doc Current UNIX epoch timestamp in integer milliseconds.
%%      Equivalient to <code>now_to_msec(os:timestamp())</code>.
-spec timestamp() -> integer().
timestamp() ->
    now_to_msec(os:timestamp()).

%% @doc Converts given time of now() format to UNIX epoch timestamp in
%%      integer milliseconds.
-spec now_to_msec(calendar:t_now()) -> integer().
now_to_msec({MegaSecs, Secs, MicroSecs}) ->
    trunc(((MegaSecs * ?MEGA) + Secs + (MicroSecs / ?MEGA)) * ?KILO).

-spec now() -> calendar:t_now().
now() ->
    erlang:now().
