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
