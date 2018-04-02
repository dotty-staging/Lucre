package de.sciss.lucre.stm

import scala.concurrent.stm.InTxn
import scala.language.higherKinds

object ThirdAttempt {
  trait DataInput

  object Reader {
    implicit object BooleanReader extends ImmutableReader[Boolean] {
      def read(in: DataInput): Boolean = ???
    }
  }

  trait Reader[-T, -Acc, +A] {
    def read(in: DataInput, access: Acc)(implicit tx: T): A
  }

  trait ImmutableReader[A] extends Reader[Any, Any, A] {
    final def read(in: DataInput, access: Any)(implicit tx: Any): A = read(in)

    def read(in: DataInput): A
  }

  trait Txn

  trait Var[T, A] {
    def apply()(implicit tx: T): A
    def update(v: A)(implicit tx: T): Unit
  }

  /** Not necessarily transactional */
  trait Base {
    type Tx
    type Var[A] <: ThirdAttempt.Var[Tx, A]
    type Acc
    type Id

    def newId()(implicit tx: Tx): Id

    def newVar[A](id: Id, init: A)(implicit tx: Tx, reader: Reader[Tx, Acc, A]): Var[A]
  }

  /** Transactional */
  trait Sys extends Base {
    type Tx <: ThirdAttempt.Tx
    type Id <: ThirdAttempt.Identifier
  }

  trait Identifier

  trait Tx {
    def peer: InTxn
  }

  trait Comp[S0 <: Sys] {
//      @elidable(elidable.ALL)

    val S: S0

    type $ = S.type
  }

  trait Test[S <: Sys] extends Comp[S] {

    def test()(implicit tx: S.Tx, r: Reader[S.Tx, S.Acc, Int]): Unit = {
      val id = S.newId()
      S.newVar(id, 1234)
    }

    def test2()(implicit tx: S.Tx): Unit = {
      val id = S.newId()
      S.newVar(id, true)
    }
  }

  trait Foo[S <: Sys] extends Comp[S] {
    def vr: S.Var[Int]

    def bar()(implicit tx: S.Tx): Int = vr()

    def baz(v: Int)(implicit tx: S.Tx): Unit =
      vr() = v

    implicit def reader: Reader[S.Tx, S.Acc, Foo[S]]

//    val * : $ = S

    def test()(implicit tx: S.Tx): Unit = {
      val id  = S.newId()
      val x   = S.newVar(id, this)
      println(x)
    }

    def okTryThis(that: Foo[$])(implicit tx: S.Tx): Int = that.bar()
  }

  trait Bar[S <: Sys] extends Comp[S] {
    def foo1: Foo[$] // Foo[S]
    def foo2: Foo[$] // Foo[S]

    def test()(implicit x: S.Tx): Unit =
      foo1.okTryThis(foo2)
  }
}