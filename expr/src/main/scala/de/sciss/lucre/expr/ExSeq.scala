/*
 *  ExSeq.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2018 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr

import de.sciss.lucre.aux.{Aux, ProductWithAux}
import de.sciss.lucre.event.{IEvent, IPull, ITargets}
import de.sciss.lucre.event.impl.IEventImpl
import de.sciss.lucre.expr
import de.sciss.lucre.stm.Sys
import de.sciss.model.Change

import scala.collection.immutable.{Seq => ISeq}

object ExSeq {
  private final class Expanded[S <: Sys[S], A](elems: ISeq[IExpr[S, A]])(implicit protected val targets: ITargets[S])
    extends IExpr[S, ISeq[A]] with IEventImpl[S, Change[ISeq[A]]] {

    def init()(implicit tx: S#Tx): this.type = {
      elems.foreach { in =>
        in.changed ---> changed
      }
      this
    }

    def value(implicit tx: S#Tx): ISeq[A] = elems.map(_.value)

    def dispose()(implicit tx: S#Tx): Unit = {
      elems.foreach { in =>
        in.changed -/-> changed
      }
    }

    def changed: IEvent[S, Change[ISeq[A]]] = this

    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Change[ISeq[A]]] = {
      val beforeB = ISeq.newBuilder[A]
      val nowB    = ISeq.newBuilder[A]
      elems.foreach { in =>
        val evt = in.changed
        val opt: Option[Change[A]] = if (pull.contains(evt)) pull(evt) else None
        opt match {
          case Some(Change(beforeV, nowV)) =>
            beforeB += beforeV
            nowB    += nowV
          case None =>
            val v = in.value
            beforeB += v
            nowB    += v
        }
      }
      val ch = Change(beforeB.result(), nowB.result())
      if (ch.isSignificant) Some(ch) else None
    }
  }
}
final case class ExSeq[+A](elems: Ex[A]*) extends Ex[ISeq[A]] with ProductWithAux {
  // $COVERAGE-OFF$
  private def simpleString: String = {
    val xs = elems.iterator.take(5).toList
    val es = if (xs.lengthCompare(5) == 0) xs.init.mkString("", ", ", ", ...")
    else xs.mkString(", ")
    s"Pat($es)"
  }

  override def toString: String = simpleString
  // $COVERAGE-ON$

  def expand[S <: Sys[S]](implicit ctx: Ex.Context[S], tx: S#Tx): IExpr[S, ISeq[A]] = {
    import ctx.targets
    val elemsEx: ISeq[IExpr[S, A]] = elems.iterator.map(_.expand[S]).toList
    new expr.ExSeq.Expanded(elemsEx).init()
  }

  def aux: scala.List[Aux] = Nil
}
