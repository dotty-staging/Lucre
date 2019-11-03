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
and the `IExpr` must hold a counter or something similar. So that `ExMap` expansion, when `value` is called, will
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

-------

# Notes 190811 - runWith

Right now, `"key".attr[A]` (in `ExOps`) requires an `Obj.Bridge[A]` which describes one method:

    def cellView[S <: Sys[S]](obj: stm.Obj[S], key: String)(implicit tx: S#Tx): CellView.Var[S, Option[A]]

and the implementation then relying on `obj.attr` and an instance of `Type.Expr` for matching. When the `Attr` is
used as an `Ex`, we only care for `CellView[S#Tx, Option[A]]` with `value` and `react`. If we introduced an
extension method `orElse` for a cell-view representing an option, we just need to introduce a cell-view
representation for `Context.Attr` or `MapLike[S, String, Form]`.

# Notes 190821 - nested keys

If we had a `CellView[S#Tx, Option[stm.Obj[S]]]`, we could start from the head sub-key with that, and then
introduce a `flatMap` foreach next sub-key which is either producing another `Option[stm.Obj[S]]` if it is in
inner position, or the final `Option[A]` if it is in last position. It requires a change in `Bridge`, where
`cellView(obj, key)` does not yet define a bidirectional constraint. (Or perhaps that doesn't even matter for the
`flatMap`).

Something along the lines of

```

if (isNested) {
  val head :: tail = key.split(":").toList
 
  def loop(parent: CellView[S#Tx, Option[stm.Obj[S]]], rem: List[String]): CellView[S#Tx, Option[A]] =
    rem match {
      case last :: Nil => 
        parent.flatMap(child => CellView.attrMap[A](child, last))
      
      case sub :: tail => 
        val childView = parent.flatMap(child => CellView.attrMap[stm.Obj[S]](child, sub))
        loop(childView, tail)

      case Nil => throw new IllegalArgumentException    // can we avoid this?
    }

  val ctxHead: CellView[S#Tx, Option[stm.Obj[S]]] = ???
  val objHead: CellView[S#Tx, Option[stm.Obj[S]]] = ???
  val ctxFull = loop(ctxHead, tail)
  val objFull = loop(objHead, tail)
  ctxFull orElse objFull

} else {
  // and again here we could implement the logic in the caller,
  // like `bridge.ctxCellView() orElse ctx.selfOption.fold(CellView.empty)(bridge.objCellView))`
  bridge.cellView()
}

```

-------

# Notes 191026 - mapping a sequence of expressions to actions

The problem arises from the impedance mismatch of `Ex[Act]` - the expression assumes that its value is
immutable, but `Act` itself must first be expanded, it's on the same level as `Ex`, it's not a primitive value.

I think we should eliminate `Ex[Act]` altogether. Also `Ex[Option[Act]]` and `Ex[Seq[Act]]`. Otherwise we might
have

```
val exSeq: Ex[Seq[Int]] = ???
val actSeq: Ex[Seq[Act]] = exSeqMap(PrintLn(_))
val last = actSeq.lastOption.orNop
LoadBang() ---> last
```

It's easy to see the problem here: `.lastOption` would work for any `Ex[Seq[_]]`, and so it would rely on
`value` to generate a `Seq[_]` first. Now we actually have only a single action, like `PrintLn(MapIt(...))`.
It can only be reasonably invoked with the entire sequence, taking care of iterating the sequence input
elements.

Or... we introduce a type next to `Act` that is more "primitive" not a `Lazy`. E.g. `Ex[Seq[ActPerform]]` or
whatever name? What happens to `PrintLn(in: Ex[String])`? More dangerous are objects which mix `Ex` with `Act`,
there are some I believe? For example `Obj.Copy`.

```
case class MapExSeqAct[A](in: Ex[Seq[A]], it: It[A], closure: Graph, fun: Act) extends Ex[Seq[ActPerform]] {

  def expand[S]: IExpr[S, Seq[ActPerform]] = ...
}

trait ActPerform {
  def execute(): Unit
}

class ExpandedMapExSeqAct[S](in: IExpr[S, Seq[A]], it: It.Expanded[S, A], closure: Graph, fun: Act)
  extends IExpr[S, Seq[ActPerform]] {

  private class PerformImpl(inV: A) {
    def executeAction(): Unit = {
      it.set(inV)
      val disp = ctx.nested {
        val funEx = fun.expand
        funEx.execute()
      }
      disp.dispose()
    }
  }

  def value: Seq[ActPerform] = {
    val inV = in.value
    inV.map(new PerformImpl)
  }
}
```

The only difference here between `Act` and `ActPerform` is that we don't register the instance creation through
`Lazy`... Which begs the question, why `Act` needs to extend `Lazy` at all? I think it is to ensure that the 
expanded `IAction` is the same with respect to registered triggers. This in turn could mean that a distinction
between `Act` and `ActPerform` _is_ useful.

It also means we should avoid implicit conversion from `Ex[ActPerform]` to `Act`?

Does `Ex[Seq[Act{Perform}]]` make sense at all? I.e. does it make sense to manipulate that sequence as a sequence,
e.g. `.reverse`? I don't think so.

We should see `map` and `flatMap` here as actually doing a `foreach`. It's also not clear if we really need
`Ex[Option[Act]]`. Instead we might either have an `Act.Option <: Act` with additional functionality, and/or we
have `Ex[Option[A]].fold(ifEmpty: Act)(df: A => Act): Act`, perhaps give it a distinct name like `foldAct`, or
`cond`.

So let us review were we ever have `Ex[Act]` and `Ex[F[Act]]`:

```
implicit def flatten(in: Ex[Act]): Act = Flatten(in)

def orNop

SeqImpl (publicly only a plain Act)

CanMapActOption

implicit object act extends Value[Act] -- and thus Const(act)
```

What happens to for comprehensions:

```
  val act = for {
    a <- aOpt
    b <- bOpt
    if guard
  } yield {
    PrintLn(a.toStr ++ b.toStr)
  }
```

That translates to

```
  aOpt.flatMap { (a: Ex[A]) => bOpt.map { (b: Ex[B]) => ... => PrintLn(...)))
```

So we need a map from `Ex[Option[A]]` to `Act`, and a flat-map from `Ex[Option[A]]` to `Act` or `Act.Option`?

---------

# Notes 191029 - issue no. 20

Here is the problematic example:

```
  val in1: Ex[Seq[Double]]  = "in1".attr(Seq.empty[Double])
  val in2: Ex[Double]       = "in2".attr(0.0)
  val out = in1.map(_ + in2)
  val b   = LoadBang()
  val c   = out.changed
  val t   = b | c
  t ---> PrintLn(Const("out = ") ++ out.mkString(", "))
```

It does not react to changes in `in2`, because `in2` is only referred to from the closure of `in1.map` which can
only be expanded when the iteration variable has been set.

Even if `in2` was collected and expanded in the outer environment, it would not propagate its changes, because the
event listeners are not registered. It appears that we need an intermediate stage, where expanding the `Map`
operation also "partially expands" the closure result expression. In the sense that we must prevent that the `value`
of `It` is used in initialization code. Should this be the case at all anywhere? Where is `Caching` used?

- in `Folder` because the peer does not emit `Change` instances. (This seems unproblematic, but also unnecessary,
  as we can calculate the `before` values from the peer events nevertheless)
- in `Timeline` (SP), probably for similar reasons
- in `DropTarget` (LucreSwing), for storing default values
- in `TimeStamp`; a bit odd the whole thing, is this ever triggered? Also stores just system time, does not
  call `value` on any inputs
- in `Obj.make`, because we also cannot get `Change` instances here
- map and flat-map stuff; this is what we are working on here, so we can assume this is "axed" anyway

So from what I can see, there is actually no case where `value` is initially called. Is it not the best idea then
to simply expand the closure results directly with the containing object (e.g. map)? Then should we removing firing
from `It.Expanded`? If we don't do that, we may also have the containing object be "deaf" to the closure result
during iteration.

But how do we generate a `Change[Seq[A]]` if an expression from the outside changes and the event reaches the
closure result? It appears that the map and flat-map object necessarily have to be `Caching`...?

```
                  env
                   |
    seq . map      V "fire" (Change)

                  res ---- res0, res1, res2   (iteration)

```

We need to fundamentally interfere in the event dispatch. If we "redirected" the `env.changed` event to 
`seq.map` instead of `res`, and we'd call `res.value` to gather the sequences, we must provide a surrogate for
`env` such that the map can "set" a `before` and `now` value. __Not nice__.

Also, we can create a very tricky structure:


```
  val env: Ex[A]
  val in: Ex[Seq[B]]

  def combine(a: Ex[A], b: Ex[B]): Ex[B]

  val m = in.map { b =>
    val c = combine(env, b)
    c.changed ---> PrintLn(c.toStr)
    c
  }

```

Although we could just say, side-effects inside closures are undefined behaviour. Let's say that. So:

```
  val env: Ex[A]
  val in: Ex[Seq[B]]

  def combine(a: Ex[A], b: Ex[B]): Ex[B]

  val m = in.map { b =>
    combine(env, b)
  }

```

Note that `env` does not need to be lexically defined outside the closure. It could as well be

```
  val in: Ex[Seq[B]]

  def combine(a: Ex[A], b: Ex[B]): Ex[B]

  val m = in.map { b =>
    val env: Ex[A] = "env".attr(default)
    combine(env, b)
  }

```

And we would have the same situation/problem. Thus there is no use to contemplate about determining the
"location" of `env`.

What we need is, if we conceptually think that `combine.changed ---> in.map.changed`, a way to __multiply__ the
`pull` calls to `combine.changed`. Likewise, if the map operation was over an `Option`, we also need to "multiple"
the pull calls - either there is one, or there is zero ("swallowed").

Say we assume there are no side-effects inside the closure; then there is no leaf in the event dispatch inside the
closure. Instead let's assume we have set up the `combine.changed ---> in.map.changed`, and thus in the _push_ phase
of event dispatch, we will reach `in.map.changed`, and that will be necessarily the _only child_ of `combine.changed`.
And thus we will encounter `pullUpdate` for `in.map.changed` at some point, and certainly before `combine.changed`.
Now `IPush.Impl.apply` is implemented by caching the results of `pullUpdate`. What we could therefore do is
isolate the call to `push.apply(combined.changed)` so that it's result is not cached and we can call `push.apply`
multiple times. As a negative side-effect, this will also prevent caching of any upstream events that are actually
drawn in from outside the closure.

However, taking first the case where `It` is _not subject to the event_, `combine.changed` would need to call
`it.value` in its `pullUpdate` body. We already have a `ThreadLocal` for the pull, because of the stupid caching.
So `it.value` could signalise the special state to the current pull, and mark the current event under consideration
as non-cached. In the second case where `It` is part of the event path, its `pullUpdate` and likewise mark the
parent event as non-cached.

In pseudo-code:

```
trait MapExpanded {
  def pullUpdate = {
    val chIn: Change[Seq[A]] = {
      val opt = if (pull.contains(in)) pull(in) else None
      opt.getOrElse {
        val inV = in.value
        Change(inV, inV)
      }
    }
    val bBefore = Seq.newBuilder[A]
    val bNow    = Seq.newBuilder[A]
    if (pull.contains(closureRes)) {
      chIn.now.foreach { e =>
        it.set(e)
        val opt = pull(closureRes) else None
        val itCh = opt.getOrElse {
          val itV = closureRes.value
          Change(itV, itV)
        }
        bBefore += itCh.before
        bNow    += itCh.now
      }
    } else {
      chIn.now.foreach { e =>
        it.set(e)
        val itV = closureRes.value
        bBefore += itV
        bNow    += itV
      }
    }
    val ch = Change(bBefore.result(), bNow.result()
    if (ch.isSignificant) Some(ch) else None
  }
}
```

This leaves one crucial component - a change in sequence size. How can we handle this without resorting to
`Caching`?' For __example__, consider:

```
val sq: Ex[Seq[Int]]
val out = sq.map { e =>
  e + sq.size
}
```

Then if `sq` changes from `Seq(1, 2, 3)` to `Seq(1, 2)`, `out` changes from `Seq(4, 5, 6)` to `Seq(3, 4)`.
We'd see a propagation for `sq` of `Change(Seq(1, 2, 3), Seq(1, 2))`, and a propagation for `sq.size` 
of `Change(3, 2)`. For the last element, there is no "now" iteration, but to obtain the `before` for `out`, we
anyway need to execute the iteration. So what happens if we provide a `Change(e, e)` for `it.changed` in that
particular iteration, using a new hook in the `IPull` API. We could thus bypass an `isSignificant` check at that
point. However, the `pull(closureRes)` might decide to return a `None` if the corresponding change is not
significant __!__.

A possible way out of this situation, is to add two methods to the pull API:

- `def applyBefore[A](source: IChangeEvent[S, A]): Option[A]`
- `def applyNow   [A](source: IChangeEvent[S, A]): Option[A]`

with

```
trait IChangeEvent[S <: Base[S], +A] extends IEvent[S, Change[A]] {
  def pullBefore(pull: IPull[S])(implicit tx: S#Tx): A
  def pullNow   (pull: IPull[S])(implicit tx: S#Tx): A
}
```

and

```
trait IChangePublisher[S <: Base[S], +A] extends IPublisher[S, Change[A]]

trait IExpr[S <: Base[S], +A] extends IChangePublisher[S, A]
```

We have to think carefully if this solves the issue, otherwise we have a lot of work for nothing...
Essentially we have to triplicate each event implementation :-/ There are 26 implementations of
`pullUpdate` in the current code base.

Thus

```
trait MapExpanded {
  def pullUpdate = {
    val chIn: Change[Seq[A]] = {
      val opt = if (pull.contains(in)) pull(in) else None
      opt.getOrElse {
        val inV = in.value
        Change(inV, inV)
      }
    }
    val bBefore = Seq.newBuilder[A]
    val bNow    = Seq.newBuilder[A]
    if (pull.contains(closureRes)) {
      chIn.before.foreach { e =>
        it.set(e)
        val itV = pullBefore(closureRes)
        bBefore += itCh.before
      }
      chIn.now.foreach { e =>
        it.set(e)
        val itV = pullNow(closureRes)
        bNow += itCh.now
      }
    } else {
      chIn.now.foreach { e =>
        it.set(e)
        val itV = closureRes.value
        bBefore += itV
        bNow    += itV
      }
    }
    val ch = Change(bBefore.result(), bNow.result()
    if (ch.isSignificant) Some(ch) else None
  }

  def pullBefore = {
    val inSeqV: Seq[A] = if (pull.contains(in)) pullBefore(in) else in.value
    val bBefore = Seq.newBuilder[A]
    val pullC = pull.contains(closureRes)
    inSeqV.foreach { e =>
      it.set(e)
      val itV = if (pullC) pullBefore(closureRes) else closureRes.value
      bBefore += itCh.before
    }
    bBefore.result()
  }
}
```

Perhaps we can avoid the duplication of `pullBefore` and `pullNow`, using only `pullChange` and marking in `Pull`
whether we are in `before` or `now` phase (also add: `resolveChange`). Then we might even drop most implementations
of `pullUpdate` by mixing in a default implementation that just calls `pull.applyBefore` and `pull.applyNow` and
returns the `Change` with `isSignificant` check. For example, `Attr`:

```
  def pullChange(pull: IPull[S])(implicit tx: S#Tx): A = {
    val dch = default.changed
    if (pull.contains(dch) && ref.get(tx.peer).isEmpty) {
      pullChange(dch)
    } else if (pull.isOrigin(this)) {
      pull.resolveChange
    } else {
      this.value
    }
  }
```

The last branch may be surprising, but perhaps we can indeed do without returning `Option[A]`, as practically all
sinks of expressions use `(if (pull.contains(source) pull(source) else None).getOrElse(source.value)`.
There are exceptions, like `BinaryOp`, so the question is if wrapping everything in `Option` indeed gives us a
performance advantage over having to calculate the binary-op value every time. I suspect that this is negligible,
and code becomes much more concise, which is also a big advantage.

-- I would say, we take the risk and remove the `Option` for `pullChange`.

Let's see how this works with caching expressions, such as `Latch`:

```
def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Change[A]] =
  if (pull(trig.changed).isEmpty) None else {
    val newValue  = in.value
    val oldValue  = ref.swap(newValue)
    if (oldValue == newValue) None else Some(Change(oldValue, newValue))
  }
```

Here we _do need_ to distinguish 'before' and 'now', so can't just define `pullChange`. Perhaps we add a `Boolean`
argument to `pullChange` or add a way to query it on the `pull` argument:

```
def pullChange(pull: IPull[S])(implicit tx: S#Tx): A =
  if (pull.isBefore || pull(trig.changed).isEmpty) ref() else {
    val newValue  = init.value
    ref() = newValue
    newValue
  }
```

So if `pullUpdate` calls `pullChange` twice, it will first return `ref()` and then return either `ref()` or the
new value.

---------------------------

We need to codify the partial knowledge of before/now values in the internal `pullMap`. We can no longer assume
we don't know anything about the value type, therefore it may be good to introduce a mutable replacement for
`Change`.

--------------------------

## Notes 191102

We missed an important case where the seq over which we map or flat-map contains mutable element which must be
traced:

```
  val f = "folder".attr[Folder](Folder())
  val children = f.children
  val names0 = children.flatMap { obj =>
    obj.attr[String]("name")
  }
  LoadBang() ---> PrintLn(names0.mkString(" "))

```

Here the attribute map is not communicating to the `graph.Obj` at all, thus the `Obj.AttrExpanded` has to create
views when it expands, and thus it must call `it.value`, which fails. I seems it's not tenable to treat the
closure function as _one_ expanded instance. For this example to work, we _must_ allow the creation of initial
state for the expanded closure, I don't see any other way? That would render the previous approach of wiping the
cache futile. Luckily, we _can_ in principle call `expand` multiple times; we just run into a similar situation, we
need to detect the expansion of `It` and use that as a stopping point in a `nonCaching` run, only this time the
cache of the graph builder needs to be wiped, not of the `IPull` phase.

Also, the `graph.Folder.children` expression is wrong; it never tracks additions and deletions from the folder.

The root of the problem is that `graph.Obj` doesn't stick with the rest because it points to a mutable structure and
is not strictly a "value". Yes, we can say there is an object identity, and in that sense it works.

So... are we going back to `ctx.nested` then? Yes and not. The mistake is that it will encapsulate expansions from
the outer environment as well, which tends to be wrong (at least if there are triggers involved). So it should be
`ctx.nonCached(it) { ... }`

How does this relate to `Caching` (does it at all?). How to the before/now phase? They might be related where there
has to be stored the 'before' state.

--------

How to create and dispose the iterations? We going exactly the opposite way as before, we need to call `.value` now
for `in` in map-expanded initialisation. So we need to ensure that it's always possible and valid to call `.value` on
an expression in expansion initialisation. And then `tryPull` is coming back to haunt as, as now we will need to
check in any `.value` that is cached whether we currently in an event dispatch :-/

Correct end points for `ctx.nested`: Instead of push/pop of new maps, we should adopt a behaviour similar to `IPull`,
such that we push/pop elements on a stack during actual expansion in the `visit` (`val exp = init`).

How about nested cases?

```
  val outer = in.flatMap { xs =>
    xs.map { x =>
      f(x)
    }
  }
```

The `xs.map` would keep its own `Disposable` with all the expansion of `f(x)`. When `outer` is disposed, so will
be the multiple expansions of `xs.map` and in turn their own disposables.

It is theoretically possible that an inner closure refers to the `It` of an outer closure (only or "first"), like

```
  val outer = in.flatMap { xs =>
    xs.map { _ =>
      f(xs)
    }
  }
```

this would translate to

```
  FlatMap.Expanded(in, it1) {
    ctx.nested(it1) {
      Map.Expanded(it1, it2) {
        ctx.nested(i2) {
          f(it1)
        }
      }
    }
  }
```

the inner map.expand already calls `in.expand` and thus `it1.expand`, before even initialising the body.
It could also have been

```
  val outer = in.flatMap { xs =>
    ys.map { _ =>
      f(xs)
    }
  }
```

Then we'd encounter the terminal symbol `it1` (`xs`) in an a nested block that uses its own terminal `it2`.
How do we roll this back correctly, how do we produce the correct disposables?

With something from the environment

```
  val a = "key".attr(0)
  val outer = in.flatMap { xs =>
    ys.map { y =>
      xs + a
    }
  }
```

Obviously we must exclude `a` from multiple expansion, and `xs + a` must be expanded per element in `in`, whereas
`ys` does not play a role in multiple expansion. I think this is a good example embodying most possible problems,
so let's traverse that, by way of `outer.expand`:

- before instantiating the class it will call `in.expand` and its own `it.expand` (`xs`). 
  Both are new to the context and thus expand and memoise the result
- the expanded flat-map in its initialisation will call `in.value` and then `ctx.nested(xs) { ... }`
- the context will thus push `xs` on its internal stack of terminal symbols.
- we will then expand the closure `ys.map { ... }`. It's new and thus will proceed to expand.
- before instantiating `ys.map` the class it will call `ys.expand` and its own `it.expand` (`y`).
  Both are new and thus expanded and memoised.
- the expanded map in its initialisation will call `ys.value` and then `ctx.nested(y) { ... }`.
- the context will thus push `y` on its internal stack of terminal symbols.
- we will then expand the closure `BinaryOp(Plus, xs, a)`. It's new and thus will proceed to expand.
- before instantiating the class it would call `xs.expand` and `a.expand`. The first would be found by the context
  on its stack of terminal symbols. We now need to mark the call sequence from `ctx.nested(xs)` to here for
  non-caching on the return of `ctx.nested(xs)`. What's in that sequence? `ys` was completed, `y` was completed.
  the open symbols are `ys.map`, `BinaryOp` and `xs`. The latter being the special `It` terminal object is ignored. 
  In other words `ys.map` and `BinaryOp` are the only things marked for transferal/removal when returning from
  `ctx.nested(xs)`.
- in contrast `a` is not a terminal symbol, and thus it will be expanded and not marked for transferal/removal.
- we then return with the expanded binary-op, which has been memoised for now?
- let's assume there are two elements in both `in` and `ys`.
- thus we enter the second call to expand `BinaryOp(Plus, xs, a)` and should find the memoised version, and return
  quickly.
- `ctx.nested(y)` ends; there are no objects marked for the terminal symbol `y`. Consequently the disposable is
  empty
- `ys.map` expansion is finished, and memoised and returned.
- Now `ctx.nested(xs)` is done; it will return `ys.map` and `BinaryOp` as its disposables, and they will no longer
  be found on the cache. Thus in the second iteration of `xs`, the above process repeats.

How do multiple paths coexist? Let's look at a slightly more complex example with two different markings:

```
  val a = "key".attr(0)
  val outer = in.flatMap { xs =>
    ys.map { y =>
      // BinaryOp(Plus, BinaryOp(Plus, xs, a), y)
      (xs + a) + y
    }
  }
```

It's obvious that the detection must work in parallel, as we could have written 
`BinaryOp(Plus, y, BinaryOp(Plus, xs, a))` and should obtain the same markings. In the first variant, we'll first
mark both inner and outer binary operators for `xs`, then the outer operator for `ys`. In the second variant, we'll
first mark the outer operator for `ys`, then (only) the inner for `xs`.
If we had `BinaryOp(Plus, xs, BinaryOp(Plus, y, a))`, we would first mark the outer operator for `xs`, then both
the inner and the outer for `y`. So we need a way to determine that `y` is nested inside `xs`.

How do we operationalise this? When doing a `ctx.visit` where the value is not cached, we need to wrap around the
call to 

```
  val exp = init
  sourceMap.transform(m => m + (ref -> exp))
```

"Marking" here could mean that we maintain a `scala.List` of terminal symbols. Before calling `init`, we store the list
and replace it temporarily by `Nil`. When we return from `init`, we
check that list. If it is still empty, we store the memoised value in the global source-map. Otherwise we add it to
the source-map corresponding to the terminal symbol. Then we "unite" the new symbols with previous symbols; if
the previous list was empty, we leave the new list, otherwise need a correct merger.

Another run through:

```
  val a = "key".attr(0)
  val outer = in.flatMap { xs =>
    ys.map { y =>
      (xs + a) + y
    }
  }
```

- first iteration of `y`
- begin `in.flatMap.expand` : is new
- `in.expand` : is new ; no terms -> global cache
- `xs.expand` : is new ; no terms -> global cache
- first iteration of `xs`
- enter `ctx.nested(xs.ref)`; terminal symbols: `xs :: Nil`
- begin `ys.map.expand` : is new
- `ys.expand` : is new ; no terms -> global cache
- `y.expand` : is new ; no terms -> global cache
- first iteration of `y`
- enter `ctx.nested(y.ref)`; terminal symbols: `y :: xs :: Nil`
- begin `BinaryOp(Plus, BinaryOp(Plus, xs, a), y).expand` : is new
- begin `BinaryOp(Plus, xs, a).expand` : is new
- `xs.expand` : in cache; is terminal symbol ; add it to marks, becoming `xs :: Nil` (prepend)
- `a.expand` : is new ; no terms -> global cache
- end `BinaryOp(Plus, xs, a).expand`. Mark `xs` found; -> `xs`'s specific cache
- previous marks was empty; new united marks is `xs :: Nil` (concat)
- `y.expand` : in cache; is terminal symbol ; add it to marks, becoming `y :: xs :: Nil` (prepend)
- end `BinaryOp(Plus, BinaryOp(Plus, xs, a), y).expand`; Marks `y :: xs :: Nil` found; -> `y`'s specific cache
- exit `ctx.nested(y.ref)`; terminal symbols and marks reverted to `xs :: Nil`. Sole disposable is
  `BinaryOp(Plus, BinaryOp(Plus, xs, a), y).expand`
- second iteration of `y`
- enter `ctx.nested(y.ref)`; terminal symbols: `y :: xs :: Nil`
- begin `BinaryOp(Plus, BinaryOp(Plus, xs, a), y).expand` : is new
- begin `BinaryOp(Plus, xs, a).expand` : is in `xs`'s specific cache; add `xs` to marks (prepend)
- `y.expand` : in cache; is terminal symbol ; add it to marks (prepend)
- end `BinaryOp(Plus, BinaryOp(Plus, xs, a), y).expand`; Marks `y :: xs :: Nil` found; -> `y`'s specific cache
- exit `ctx.nested(y.ref)`; terminal symbols and marks reverted to `xs :: Nil`. Sole disposable is
  `BinaryOp(Plus, BinaryOp(Plus, xs, a), y).expand` 
- end `ys.map.expand` : Mark `xs` found; -> `xs`'s specific cache
- exit `ctx.nested(xs.ref)`; terminal symbols and marks reverted to `Nil`. Disposables are
  `ys.map.expand` and `BinaryOp(Plus, xs, a).expand`
- second iteration of `xs`...

Variant:

```
  val a = "key".attr(0)
  val outer = in.flatMap { xs =>
    ys.map { y =>
      y + (xs + a)
    }
  }
```

- first iteration of `y`
- begin `in.flatMap.expand` : is new
- `in.expand` : is new ; no terms -> global cache
- `xs.expand` : is new ; no terms -> global cache
- first iteration of `xs`
- enter `ctx.nested(xs.ref)`; terminal symbols: `xs :: Nil`
- begin `ys.map.expand` : is new
- `ys.expand` : is new ; no terms -> global cache
- `y.expand` : is new ; no terms -> global cache
- first iteration of `y`
- enter `ctx.nested(y.ref)`; terminal symbols: `y :: xs :: Nil`
- begin `BinaryOp(Plus, y, BinaryOp(Plus, xs, a)).expand` : is new
- `y.expand` : in cache; is terminal symbol ; add it to marks
- begin `BinaryOp(Plus, xs, a).expand` : is new (pushed marks is `Nil`)
- `xs.expand` : in cache; is terminal symbol ; add it to marks (pushed marks is `xs :: Nil`) (prepend)
- `a.expand` : is new ; no terms -> global cache
- end `BinaryOp(Plus, xs, a).expand`. Mark `xs` found; -> `xs`'s specific cache
- previous marks was `y :: Nil`; new united marks is `y :: xs :: Nil` (concat)
- end `BinaryOp(Plus, y, BinaryOp(Plus, xs, a)).expand`; Marks `y :: xs :: Nil` found; -> `y`'s specific cache
- exit `ctx.nested(y.ref)`; terminal symbols and marks reverted to `xs :: Nil`. Sole disposable is
  `BinaryOp(Plus, y, BinaryOp(Plus, xs, a)).expand`
- second iteration of `y`
- enter `ctx.nested(y.ref)`; terminal symbols: `y :: xs :: Nil`
- begin `BinaryOp(Plus, y, BinaryOp(Plus, xs, a)).expand` : is new
- `y.expand` : in cache; is terminal symbol ; add it to marks, becoming `y :: Nil` (concat)
- begin `BinaryOp(Plus, xs, a).expand` : is in `xs`'s specific cache; add `xs` to marks, thus `y :: xs :: Nil` (__not prepend__)
- end `BinaryOp(Plus, y, BinaryOp(Plus, xs, a)).expand`; Marks `y :: xs :: Nil` found; -> `y`'s specific cache
- exit `ctx.nested(y.ref)`; terminal symbols and marks reverted to `xs :: Nil`. Sole disposable is
  `BinaryOp(Plus, y, BinaryOp(Plus, xs, a)).expand` 
- end `ys.map.expand` : Mark `xs` found; -> `xs`'s specific cache
- exit `ctx.nested(xs.ref)`; terminal symbols and marks reverted to `Nil`. Disposables are
  `ys.map.expand` and `BinaryOp(Plus, xs, a).expand`
- second iteration of `xs`...

Question: When marks are united, must that be sorted, or just `old ++ new` ? Adding marks surely must be sorted.
