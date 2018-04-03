package de.sciss.lucre.stm

import scala.language.higherKinds

object FourthAttempt {
  trait Sys {
    type Tx
    type Acc
    type Vr[A]

    def newVar[A](init: A)(implicit tx: Tx, r: Reader[Tx, Acc, A]): Vr[A]
    def read[A](v: Vr[A])(implicit tx: Tx): A
    def update[A](v: Vr[A], a: A)(implicit tx: Tx): Unit
  }

  trait Reader[-Tx, -Acc, A] {
    def read(tx: Tx, acc: Acc): A
  }

  trait Foo {
    val S: Sys

    def vr: S.Vr[Int]

    def bar()(implicit tx: S.Tx): Int = S.read(vr)

    def baz(v: Int)(implicit tx: S.Tx): Unit =
      S.update(vr, v)

    implicit def reader: Reader[S.Tx, S.Acc, Foo]

    def test()(implicit tx: S.Tx): Unit = {
      val x = S.newVar(this)
      println(x)
    }

    def okTryThis(that: Foo)(implicit tx: S.Tx): Int = ??? // that.bar()
  }

  abstract class Bar[S <: Base](final val S: S) extends Comp[S] {
    def foo1: Foo // Foo[S]
    def foo2: Foo // Foo[S]

    def hello()(implicit tx: S.Tx): Int

    //    def test2()(implicit tx: S.Tx): Int =
    //      attempt(this.S)(this)

    def test3()(implicit tx: S.Tx): Int =
      ??? // attempt2[$, S.Tx](this)

    def test()(implicit x: S.Tx): Unit =
      ??? // foo1.okTryThis(foo2)
  }

//  def attempt[S <: Sys](S: S)(b: Bar[S.type])(implicit tx: S.Tx): Int = b match {
//    case baz: Baz[S.type] => baz.hello()
//  }
//
//  def attempt2[S <: Base.T[T], T](b: Bar[S])(implicit tx: T): Int = b match {
//    case baz: Baz[S] => baz.hello()
//  }
}