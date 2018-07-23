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
