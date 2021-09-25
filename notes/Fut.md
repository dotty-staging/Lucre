# Fut

## notes 27-Mar-2021

Introduce a more "functional" abstraction than the odd pairing of an action
with a resulting expression.

```scala
val f: Fut[Seq[URI]] = LoadBang() --> artifact.list

// val done: Fut[Act] = f.map { sq =>
//   PrintLn(sq.mkString)
// }

val nada: Unit = f.foreach { sq =>
  PrintLn(sq.mkString)
}

f.done --> PrintLn("done")

f.failed --> PrintLn("failed: " ++ f.failure.toStr)   // ?

val f: Fut[Option[URI]] = f.map { sq => sq.headOption }

val f: Fut[URI] = f.flatMap { sq => sq.headOption }  // ?

trait IFuture[T, +A] extends IAction[T] {
// ...
}

trait Fut[+A] extends Act {
  override type Repr[T <: Txn[T]] <: IFuture[T, A]
}

val ranAll = for {
  _ <- r1.runWith("foo" -> 1)
  _ <- r2.runWith("bar" -> 2.3)
} yield ()

ranAll.done --> PrintLn("All done")

```
What to do with the existing `Act with Ex[_]`? Like `TimeStamp` / `TimeStamp.Update`.

----

## notes 25-Sep-2021

Leave "synchronous" actions like `TimeStamp.update` as they are. Leave `Fut[Unit]` candidates as `Act`, for 
instance `Delay`. Or allow both?

```scala
val d = Delay(10.0)
for(_ <- d) yield PrintLn("Hello")
```

Actually it's the `with Trig` that we're looking at here. A `Trig` behaves like `Fut[Unit]` I guess?

```scala
for {
  _ <- LoadBang()
  _ <- Delay(10.0)
} PrintLn("Hello")
```

This would translate to

```scala
(LoadBang(): Fut[Unit]).flatMap((_: Ex[Unit]) => (Delay(10.0): Fut[Unit])).foreach((_: Ex[Unit]) => PrintLn("Hello"))
```

Does that make sense?

Let's say, we can still calculate some form of default expression synchronously, like the default time stamp,
the empty OSC message etc. Then the point of `Fut` would be to determine a trigger when a new result is in, even if
the expression doesn't change, i.e. we should not rely on `.changed` as the trigger.

Whenever it is not feasible to provide a default value, we also walk away from `Trig with Ex[A]` and use `Fut[A]`
instead. Then there could always be a `toTrig` that converts `Fut[A]` to plain `Trig`, a bit like 
`concurrent.Future.mapTo[Unit]`.
