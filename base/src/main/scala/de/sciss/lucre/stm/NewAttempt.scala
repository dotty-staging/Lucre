package de.sciss.lucre.stm

import scala.language.higherKinds

object NewAttempt {
  trait Sys {
    type Tx <: Txn[this.type]
    type Acc
    type Vr[A] <: Var[Tx, A]
  }

  trait Comp[S <: Sys] {
    val S: S
    final type S1 = S.type
  }

  trait Reader[-Tx, -Acc, A] {
    def read(tx: Tx, acc: Acc): A
  }

  trait Txn[S <: Sys] extends Comp[S] {
    def newVar[A](init: A)(implicit r: Reader[S.Tx, S.Acc, A]): S.Vr[A]
  }

  trait Var[-Tx, A] {
    def apply()(implicit tx: Tx): A
    def update(v: A)(implicit tx: Tx): Unit
  }

  trait Foo[S <: Sys] extends Comp[S] {
//    final def self: Foo[S] = this.asInstanceOf[Foo[S]]

    def vr: S.Vr[Int]

    def bar()(implicit tx: S.Tx): Int = vr()

    def baz(v: Int)(implicit tx: S.Tx): Unit =
      vr() = v

    implicit def reader: Reader[S.Tx, S.Acc, Foo[S]]

    def test()(implicit tx: S.Tx): Unit = {
      val x = tx.newVar(this)
      println(x)
    }

    def okTryThis(that: Foo[S1])(implicit tx: S.Tx): Int = that.bar()
  }

  trait Bar[S <: Sys] extends Comp[S] {
    def foo1: Foo[S1] // Foo[S]
    def foo2: Foo[S1] // Foo[S]

    def test()(implicit x: S.Tx): Unit =
      foo1.okTryThis(foo2)
  }
}
