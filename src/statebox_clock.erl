%% @doc Timestamp functions for <code>statebox:new/1</code> and
%%      <code>statebox:modify/2</code>.

-module(statebox_clock).
-export([timestamp/0, now_to_msec/1, now/0]).

-type t_now() :: {integer(), integer(), integer()}.
-type timestamp() :: integer().
-export_type([t_now/0, timestamp/0]).

-define(KILO, 1000).
-define(MEGA, 1000000).

%% @doc Current UNIX epoch timestamp in integer milliseconds.
%%      Equivalient to <code>now_to_msec(os:timestamp())</code>.
-spec timestamp() -> timestamp().
timestamp() ->
    now_to_msec(os:timestamp()).

%% @doc Converts given time of now() format to UNIX epoch timestamp in
%%      integer milliseconds.
-spec now_to_msec(t_now()) -> timestamp().
now_to_msec({MegaSecs, Secs, MicroSecs}) ->
    trunc(((MegaSecs * ?MEGA) + Secs + (MicroSecs / ?MEGA)) * ?KILO).

-spec now() -> t_now().
now() ->
    erlang:now().
