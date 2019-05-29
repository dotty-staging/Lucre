# Events

The problem now is that we assume that `Event` has a `Node <: Elem` with `S#Id` and is writable.
How difficult would it be to open up the event API to allow for light in-memory events? Of course,
we could just use `Observable`, but the problem is in multiple inlet expressions, where we want a
clean mechanism to collect diamonds, such as `a + a` does not create strange behaviour.

`.node` is only used for getting `.node.targets` or for `.node.id`. `Targets` could be split into
two APIs, where the super-type only has `add` and `remove`, i.e. with `Event` in covariant position,
allowing `TargetsLike >: (TargetsI, Targets)`?

Alternatively, we simply create a parallel structure for `IEvent`. This is certainly less work.

```
trait IEvent[S <: Base[S], +A] extends Observable[S#Tx, A] {
  def ---> (sink: IEvent[S, Any])(implicit tx: S#Tx): Unit
  def -/-> (sink: IEvent[S, Any])(implicit tx: S#Tx): Unit

  def react(fun: S#Tx => A => Unit)(implicit tx: S#Tx): Disposable[S#Tx]

  private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[A]
}
```

The amount of duplication is still low, and implementation wise, it does not look like we'd run into much
DRY, because we won't be using `IdentifierMap`, deal with serialization, etc.

-------

# Map

Can we do this: `Ex[Seq[A]] => Function1[A, B] => Ex[Seq[B]]`? Let's say we rely on a structure where expression
values are not cached, so the auxiliary iterator variable of type `Ex[A]` does not need to dispatch events (well,
if it is a problem, it could still do that), so we simply poll the function result expression a number of times,
an the `IExpr` must hold a counter or something similar. So that `ExMap` expansion, when `value` is called, will
reset the iterator and poll it a number of times.

```
trait ExMap[A, B](in: Ex[Seq[A]], it: Ex[A], fun: Ex[B]) extends Ex[Seq[B]] {
  def expand = {
    val inEx = in.expand
    ...
  }
}
```

There is a cyclic dependency from `it` to `ExMap` (and vice versa). How did we solve this in Patterns?

```
  def map[B](f: Pat[A] => Pat[B]): Pat[Pat[B]] = {
    val b     = Graph.builder
    val it    = b.allocToken[A]()
    val inner = Graph {
      f(it)
    }
    PatMap(outer = x, it = it, inner = inner)
  }
  
  // where
  
  def allocToken[A](): It[A]
  
  case class It[A](token: Int) extends Pattern[A]
  
  case class PatMap[A1, A](outer: Pat[Pat[A1]], it: It[A1], inner: Pat[A]) extends Pattern[Pat[A]]
  
```

which would translate to

```
  def map[B](f: Ex[A] => Ex[B]): Ex[Seq[B]] = {
    val b     = Graph.builder
    val it    = b.allocToken[A]()
    val inner = Graph {
      f(it)
    }
    ExMap(outer = x, it = it, inner = inner)
  }
  
  // where
  
  def allocToken[A](): It[A]
  
  case class It[A](token: Int) extends Ex[A]
  
  case class PatMap[A1, A](outer: Ex[Seq[A1]], it: It[A1], inner: Pat[A]) extends Ex[Seq[A]]
  
```

There is also

```
trait ItStreamSource[S <: Base[S], A] {
  def token: Int

  def mkItStream()(implicit ctx: Context[S], tx: S#Tx): ItStream[S, A]

  def registerItStream(stream: ItStream[S, A])(implicit tx: S#Tx): Unit
}

```

where the `PatMap` expands to such `ItStreamSource`. This might be more complex and different than what we need,
because of the multiple expansion of the same pattern (`It`) into streams.

Basically, what we need to copy is the `allocToken`, and a simplified form of it-stream-input, like
`putItSource` and `popItSource`.

-------

# Notes 190524 - UndoableEdit

Some actions, such as `Attr.Set`, would be susceptible for undo management. The undo manager could
be either the one of the widget view, or one coming from outside, say from a drag-and-drop operation.
We can only dynamically look it up, say through `UndoManager.find`, it should not be passed around in
method signatures. Since the conversion from primitive type `A` to `Obj[S]` only happens within 
`CellView.attr`, the point of intervention would be the creation of the cell view in
`ExObjBridgeImpl`, so that a more elaborate cell view could be undo-management conscious.
`Folder.Append` etc. are a bit more straight forward.

Where does the undo manager come from? First, the "built-in" of the widget view. Say we have:

```
val a = "foo".attr[Int]
val b = Bang()
b ---> a.set(1234)
b
```

Clearly at any trigger link, we have a possible point for declaring the undo context.
The undo context may change, for example if we take it from a drop operation, say we enhance

```
trait TimelineViewOps(in: Ex[TimelineView]) {
  def undo: Ex[UndoManager]
}
```

If we do asynchronous processing, say FScape, we would have to latch that expression for correct later use.
It should not matter whether we latch an `Ex[UndoManager]` or an `Ex[TimelineView]`, so semantically clearer would
be the latter. Say we have stored that somehow now:

```
val undo: Ex[UndoManager] = UndoManager()  // i.e. the one of the widget view itself
val a = "foo".attr[Int]
val b = Bang()
b ---> undo.edit(a.set(1234))
b
```

The `undo` name is weird here. It should be short, though. Perhaps just `edit` directly?

```
val edit = Edit()
val a = "foo".attr[Int]
val b = Bang()
b ---> edit(a.set(1234))
b
```

And we need the option to have a compound edit name, say

```
val edit = Edit()
val a = "foo".attr[Int]
val c = "bar".attr[String]
val b = Bang()
b ---> edit("Do stuff")(
  a.set(1234),
  c.set("hello")
)
b
```

(So we encapsulate the `Act.Seq` here).


```
val r       = Runner("fsc")
val tgt     = DropTarget()
val drop    = tgt.select[TimelineView]
val tlv     = drop.value
val timed   = tlv.selectedObjects
val sr      = tlv.sampleRate

val aIn1    = Artifact("fsc:in1")
val aOut    = Artifact("fsc:out")
val ts      = TimeStamp()
val tsf     = ts.format("'render_'yyMMdd'_'HHmmss'.aif'")

val actRender  = for {
  t1 <- timed.applyOption(0)
  c1 <- t1.value.attr[AudioCue]("sig")
} yield {
  Act(
    aIn1 .set(c1.artifact),
    aOut .set(aOut.replaceName(tsf)),    
    r.run
  )
}

val stopped   = r.state sig_== 0
val errored   = r.messages.nonEmpty
val trRender  = drop.received  .filter(stopped)
val trDone    = r.state.changed.filter(stopped)
val trOk      = trDone.filter(!errored)

trRender ---> actRender.getOrElse(PrintLn("Incomplete data"))
val edit      = drop.edit latch trRender   // or tlv.edit

val specOut = AudioFileSpec.read(aOut)
  .getOrElse(AudioFileSpec.Empty())
val artOut  = AudioCue(aOut, specOut)

val fResOpt = "results".attr[Timeline]
val pRes = Proc.Tape(artOut)

val actDone = for {
  fRes <- fResOpt
} yield {
  edit("Paste rendered stuff")(
    PrintLn(Const("Rendering done. ") ++ artOut.toStr),
    pRes.make,
    fRes.add(Span((sr * 4).toLong, (sr * 10).toLong), pRes)
  )
}

trOk ---> actDone.getOrElse(PrintLn("Missing results folder"))

tgt
```

This looks ok. Not sure about `.latch`, it might be a too unfamiliar term.
Perhaps add a symbol equivalent, like `<|`.

```
val edit = drop.edit <| trRender
```

So we have

```
trait EditOps(in: Ex[Edit]) {
  def apply(name: Ex[String])(act: Act*): Act
  def apply(act: Act): Act
}
```

We could base our edit implementation on the interface used in Dotterweide, but it lacks transactional support
(and in Mellite, using `CompoundEdit`, we actually fire up `cursor.step` multiple times, which is not good.)

----

# Caching Trouble

So depending on the indeterminate order of event push, we may end up with `drop.received` arriving earlier to
a latch, for example, than to the latch's cached input, such as `drop.value`. And even if we change the reaction
`Set` to an ordered sequence, that would not guarantee correct ordering per se; after all the user sees a
"logical" program, not a procedural one. Another observation is that _every_ instance that mixes in `Caching`
ultimately is an `IExpr` with a `value` method referring to the cached reference.

There could be two approaches:

- change the `value` implementation by dynamically looking up an ongoing pull phase, "fast tracking" its resolution
  and providing ad-hoc the `Option[A]` to `IPull`. This is a bit ugly, but should certainly work
- change the ordering of reactions in the pull phase by ensuring that `Caching` objects are pulled first.
  It is unclear, though, if this guarantees that no stale values are retrieved from a caching object's dependents.
  
Let's validate the second option. Two caching expressions, `A` and `B`, with `B` depending on `A`. If the reaction
set is still a `Set`, there is no guarantee that `A` is pulled before `B`, thus is could fail. If we change that
to guarantee order, it should be correct, because `B#expand` must necessarily call `A#expand` first, so any
event listening of `A` is necessarily registered before the event listening of `B`. With perhaps one exception:
Late expansion of expression as "closures" inside `map`, `flatMap` etc.? Well these closures cannot meaningfully
react to triggers any way, they will only be invoked for their `value` things, so probably these are not causing
trouble.

So the second route is "nicer" for the `Caching` instances, but makes the event reaction maintenance more difficult,
because we might still want "fast" removal, so we need a structure that is both fast in look-up and removal, and
still ordered:

- `append`: O(1) or O(logN)
- `removal`: O(logN)

Actually... looking at `ITargets`, we already use `List` and no `Set`, so the only `Set` in the event system is
`Parents` in `IPull`. That's used in some action implementations in terms of `parents.exists(pull(_).isDefined`,
and not in terms of `contains`, so we could even make this a `List` if necessary.

So what we can do is implement the second path, as it's transparent to the expressions; and if we find that it
still has trouble correctly resolving the order of cached values, then we'll have to go for the first path.

The that worries me is that I get random problems with `Timeline.Empty` in the drop example. If the above observation
is correct, the case should either always succeed or always fail? So it must be related to `pull.parents`?
