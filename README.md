statebox - state "monad" for automated conflict resolution
==========================================================

<bob@redivi.com>

Overview:
---------

statebox is a data structure you can use with an eventually consistent
system such as riak to resolve conflicts between siblings in a deterministic
manner.

Status:
-------

Not used in production, but it has a test suite that passes. We plan to use it
soon.

Theory:
-------

A statebox wraps a current value and an event queue. The event queue is
an ordered list of `{timestamp(), op()}`. When two or more statebox
are merged with `statebox:merge/1`, the event queues are merged with
`lists:umerge/1` and the operations are performed again over the current
value of the newest statebox, producing a new statebox with conflicts
resolved in a deterministic manner.

An `op()` is a `{fun(), [term()]}`, with all but the last argument specified
in the term list. For example `{ordsets:add_element/2, [a]}`. To evaluate
this op, `ordsets:add_element(a, value(Statebox))` will be called. It is also
possible to specify an `op()` as a `{module(), atom(), [term()]}` tuple, or
as a list of `op()` when performing several operations at the same timestamp.

There are several important limitations on the kinds of `op()` that are safe
to use (`{F, [Arg]}` is the example `op()` used below):

* An `op()` must be repeatable: `F(Arg, F(Arg, Value)) =:= F(Arg, Value)`
* If the `{fun(), [term()]}` form is used, the `fun()` should be a reference
  to an exported function.
* `F(Arg, Value)` should return the same type as `Value`.

Some examples of safe to use `op()` that ship with Erlang:

* `{fun ordsets:add_element/2, [SomeElem]}` and
  `{fun ordsets:del_element/2, [SomeElem]}`
* `{fun ordsets:union/2, [SomeOrdset]}` and
  `{fun ordsets:subtract/2, [SomeOrdset]}`
* `{fun orddict:store/3, [Key, Value]}`

Some examples of functions you can not use as `op()`:

* `{fun orddict:update_counter, [Key, Inc]}` - it is not repeatable.
  `F(a, 1, [{a, 0}]) =/= F(a, 1, F(a, 1, [{a, 0}]))`

Optimizations:
--------------

There are two functions that modify a statebox that can be used to
reduce its size. One or both of these should be done every time before
serializing the statebox.

* `truncate(N, Statebox)` return Statebox with no more than `N` events in its
  queue.
* `expire(Age, Statebox)` return Statebox with no events older than
  `last_modified(Statebox) - Age`. If using `new/1` and `modify/2`, then this
  is in milliseconds.

Usage:
------

Simple `ordsets()` example:

    New = statebox:new(fun () -> [] end),
    ChildA = statebox:modify({fun ordsets:add_element/2, [a]}, New),
    ChildB = statebox:modify({fun ordsets:add_element/2, [b]}, New),
    Resolved = statebox:merge([ChildA, ChildB]),
    statebox:value(Resolved) =:= [a, b].

With manual control over timestamps:

    New = statebox:new(0, fun () -> [] end),
    ChildA = statebox:modify(1, {fun ordsets:add_element/2, [a]}, New),
    ChildB = statebox:modify(2, {fun ordsets:add_element/2, [b]}, New),
    Resolved = statebox:merge([ChildA, ChildB]),
    statebox:value(Resolved) =:= [a, b].

Using the `statebox_orddict` convenience wrapper:

    New = statebox_orddict:from_values([]),
    ChildA = statebox:modify([statebox_orddict:f_store(a, 1),
                              statebox_orddict:f_union(c, [a, aa])],
                             New),
    ChildB = statebox:modify([statebox_orddict:f_store(b, 1),
                              statebox_orddict:f_union(c, [b, bb])],
                             New),
    Resovled = statebox_orddict:from_values([ChildA, ChildB]),
    statebox:value(Resolved) =:= [{a, 1}, {b, 1}, {c, [a, aa, b, bb]}].