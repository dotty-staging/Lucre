# Bridging `Expr` and `Ex` - how to implement `IntEx` object, for instance

The main issue is event dispatch. In current behaviour, we assume a transitory in-memory view only,
where `"key".attr` upon expansion just registers an observer via `react` which will be disposed when the
`Control`/`Act`/`Widget` view closes or runner is disposed. Since now there is no real disposal of the
`IntObj` that wraps an `Ex[Int]`, we can not rely on this behaviour. If we call `react` on each deserialisation,
we end up with a multiplication of event paths, and will invoke them on changes even if there is no leaf
observer.

If we were to avoid that, we would need to restrict the `Ex[A]` to `IExpr[T, A]` expansion to happen only temporarily
during the `pullUpdate`. Then we would need to know the source events that must be connected, and that connection
happens only during instantiation, but not deserialisation. This draws into doubt the usefulness of relying on
`"key".attr`? Because that might be the result of a `map` or `flatMap` over other things and not "stable". It's 
somehow similar to creating `Proc.Output` instances depending on the graph elements. We might also want to use a
shallow search on the `Ex`; are they collected anywhere? I believe not (only `addControl`). All we could do is
"test" the expression by expanding it once and collecting information. Then perhaps a better idea is to introduce
new abstractions for inlets and outlets of an ex-object?

## Use cases

One use case would be to generate predicates for filtering or sorting contents, say of a folder. Here, this could
be thought of as in-memory only, not persisted. Another case is arithmetic expressions based on other primitive
values, e.g. keeping an `ampDb` version of a global variable. These must be persisted.

A basic question to answer is whether the inputs of the expression program should be variable or not. It could be
useful to be able to swap them out. But should their types be rigid? E.g. "always a tuple 2 expression program with
one integer and one boolean input". The `"key".attr[A](default)` syntax is also not ultimately satisfying. An
advantage, however, is that we can be more relaxed with matching the input types (e.g. converting automatically
between different numeric types). A disadvantage is having to deal with absent inputs. We could introduce a shortcut,
like `In(0)` as shorthand for `"in".attr[Int](0)`.

We'll still run into the problem - that was never addressed - of disposing objects. Say the expression program
lives inside an `IntObj.newVar`; then when the editor contents is "committed", how do we dispose the old program
to avoid that `Targets` of inputs get more and more populated with stale dependents? We cannot know that we can
safely dispose the previous version, as it might be stilled referenced in the workspace. It all boils down to the
need to implement proper garbage collection :-/

For the time being, we could just call `dispose` on the old object and put the restriction on the user to not keep
using the old version. That's not nice, but at least it would prevent the bigger problem of fastly growing targets
children lists as one develops an expression program.

----

## Approaches

Like `Pattern` and `Stream`, one could think of a serialised version of the ex programme; but the serialization
layer is made for `Ex` not `IExpr`, and the current implementations of `IExpr` would both need serialization and
storing all their inputs in handles instead of directly. Seems like an extreme amount of work to change that.

Despite the extra effort of re-expanding and disposing the ex programme upon each event pull phase, the simplest
solution for now could be to scan the ex programme for known objects such as `Attr.apply` or `Attr.WithDefault`,
see if they have constant keys, then set up the event links for those. There is already a mechanism that makes
sure we traverse the entire tree: Serialization. We just need to add a custom `RefMapOut` and match the written
expressions.

Actually, `Attr.WithDefault` already uses a constant key.

----

## An example: predicate on size of a folder

Things like the following should work: an object carrying an `Ex[Boolean]` that indicates whether a folder is
empty or not. The program would probably be `In(Folder()).nonEmpty` or `"in".attr(Folder()).nonEmpty`.

A difficulty is introduced into the very reduced `Event` model by the fact that we cannot "directly" watch for
changes of the attribute with key `"in"`; instead, there must be one default event listening for any attribute map
change; this way `Added`, `Removed`, `Replaced` can be detected and filtered. But it cannot be detected whether an
existing attribute changes, e.g. a `Folder` stored in the `"in"` attribute. The ex obj would have to add another
event connection when that folder exists; consequently, remove it when the folder disappears from the attribute
entry. Is it safe to call `--->` and `-/->` during an event dispatch? Since would happen in `pullUpdate`, all
event reactions have been collected by now, so it _should_ be safe. But it introduces the need to cache the observed
values; at least store a boolean that indicates "watching / not-watching" (as we can always find the event via
`upd.before.changed`)? Even more complicated will be a support of indirect keys, such as `"main:sub"`. In the first
implementation, we could forbid those keys and/or print a warning that they are not properly tracked.

## An example: mapping over a collection

An object carrying an `Ex[Long]` that indicates the maximum length of a list of audio cues.
The program could be `In(Folder()).children.collect[AudioCue].map(_.spec.numFrames).maxOption.getOrElse(0L)`.
Now even tracing the `changed` of the `"in"` attribute's value would not suffice; it would only detect additions and
removals of the folder, but not changes to an `AudioCue.Obj.Var` contained in it.
Essentially, such as scenario can only work if we go the route of persisting the entire expanded `IExpr` tree.
There are currently around 60 implementations of `IExpr` in Lucre alone, more in SP.

The implementation of the Pattern library's `Stream` might be a way to go this direction. To explore it, one would
probably create a branch with the `Ex` basics, plus unary and binary operations, plus attributes handling, and see
if this works or not.
