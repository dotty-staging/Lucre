/*
 *  ExSeq.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2019 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
*
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr

import de.sciss.lucre.event.impl.IEventImpl
import de.sciss.lucre.event.{IEvent, IPull, ITargets}
import de.sciss.lucre.expr
import de.sciss.lucre.expr.graph.Ex
import de.sciss.lucre.stm.Sys
import de.sciss.model.Change

object ExSeq {
  private final class Expanded[S <: Sys[S], A](elems: Seq[IExpr[S, A]])(implicit protected val targets: ITargets[S])
    extends IExpr[S, Seq[A]] with IEventImpl[S, Change[Seq[A]]] {

    def init()(implicit tx: S#Tx): this.type = {
      elems.foreach { in =>
        in.changed ---> changed
      }
      this
    }

    def value(implicit tx: S#Tx): Seq[A] = elems.map(_.value)

    def dispose()(implicit tx: S#Tx): Unit = {
      elems.foreach { in =>
        in.changed -/-> changed
      }
    }

    def changed: IEvent[S, Change[Seq[A]]] = this

    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Change[Seq[A]]] = {
      val beforeB = Seq.newBuilder[A]
      val nowB    = Seq.newBuilder[A]
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
final case class ExSeq[A](elems: Ex[A]*) extends Ex[Seq[A]] {

  type Repr[S <: Sys[S]] = IExpr[S, Seq[A]]

  private def simpleString: String = {
    val xs = elems.iterator.take(5).toList
    val es = if (xs.lengthCompare(5) == 0) xs.init.mkString("", ", ", ", ...")
    else xs.mkString(", ")
    s"ExSeq($es)"
  }

  override def toString: String = simpleString

  protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
    import ctx.targets
    val elemsEx: Seq[IExpr[S, A]] = elems.iterator.map(_.expand[S]).toList
    new expr.ExSeq.Expanded(elemsEx).init()
  }
}
