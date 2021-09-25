/*
 *  Model.scala
 *  (Lucre 4)
 *
 *  Copyright (c) 2009-2021 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr

import de.sciss.lucre.expr.graph.Ex

object Model {
  implicit def modelArrow[A]: Arrow[A, Model] = new ModelArrow[A]

  private class ModelArrow[A] extends Arrow[A, Model] {
    override def patchTo(source: Ex.Source[A], sink: Model[A]): Unit =
      sink.update(source())

    override def patchFrom(source: Model[A], sink: Ex.Sink[A]): Unit =
      sink.update(source())
  }

  implicit final class Ops[A](private val m: Model[A]) extends AnyVal {
    @deprecated("Use <-> instead", since = "4.4.5")
    def <--> [F[_]](that: F[A])(implicit left: Arrow.Left[A, F], right: Arrow.Right[A, F]): Unit = <-> [F](that)

    def <-> [F[_]](that: F[A])(implicit left: Arrow.Left[A, F], right: Arrow.Right[A, F]): Unit = {
      left  .patchFrom(that, m)
      right .patchTo(m, that)
    }

    @deprecated("Use --> instead", since = "4.4.5")
    def ---> [F[_]](that: F[A])(implicit arrow: Arrow.Right[A, F]): Unit = --> [F](that)

    def --> [F[_]](that: F[A])(implicit arrow: Arrow.Right[A, F]): Unit =
      arrow.patchTo(m, that)

    @deprecated("Use <-- instead", since = "4.4.5")
    def <--- [F[_]](that: F[A])(implicit arrow: Arrow.Left[A, F]): Unit =  <-- [F](that)

    def <-- [F[_]](that: F[A])(implicit arrow: Arrow.Left[A, F]): Unit =
      arrow.patchFrom(that, m)
  }
}
/** A model behaves like a `Ref[Ex[A]]`, that is it can give an expression,
  * and it can be "set" to an expression, which means its internal state is synchronized to another expression.
  * For example, a slider widget may contain a `Model[Int]` where the expression represents the current slider value,
  * and updating the expression synchronizes the slider to an external expression.
  *
  * Syntactic alternatives are available through the implicit `Ops`, so that one can write
  * `model <-- ex` instead of `model.update(ex)` or `model <-> attr` instead of
  * `model.update(attr); attr.update(model())`.
  */
trait Model[A] extends Ex.Sink[A] with Ex.Source[A]
