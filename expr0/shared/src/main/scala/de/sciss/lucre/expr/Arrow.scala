/*
 *  Arrow.scala
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

object Arrow {
  trait Right[A, -F[_]] {
    def patchTo(source: Ex.Source[A], sink: F[A]): Unit
  }

  object Left {
    implicit def ex[A]: Left[A, Ex] = new ExArrowLeft[A]

    private final class ExArrowLeft[A] extends Left[A, Ex] {
      override def patchFrom(source: Ex[A], sink: Ex.Sink[A]): Unit =
        sink.update(source)
    }
  }
  trait Left[A, -F[_]] {
    def patchFrom(source: F[A], sink: Ex.Sink[A]): Unit
  }

//  private[expr] final class Join[A, F[_]](left: Left[A, F], right: Right[A, F]) extends Arrow[A, F] {
//    override def patchTo  (source: Ex.Source[A], sink: F[A] ): Unit = right.patchTo  (source, sink)
//    override def patchFrom(source: F[A], sink: Ex.Sink[A]   ): Unit = left.patchFrom (source, sink)
//  }
}
trait Arrow[A, F[_]] extends Arrow.Left[A, F] with Arrow.Right[A, F]
