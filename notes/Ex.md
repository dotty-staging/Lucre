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