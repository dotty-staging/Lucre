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

import de.sciss.lucre.event.impl.IChangeEventImpl
import de.sciss.lucre.event.{IChangeEvent, IPull, ITargets}
import de.sciss.lucre.expr
import de.sciss.lucre.expr.graph.{Ex, It}
import de.sciss.lucre.stm.Sys

object ExSeq {
  private final class Expanded[S <: Sys[S], A](elems: Seq[IExpr[S, A]])(implicit protected val targets: ITargets[S])
    extends IExpr[S, Seq[A]] with IChangeEventImpl[S, Seq[A]] {

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

    def changed: IChangeEvent[S, Seq[A]] = this

    private[lucre] def pullChange(pull: IPull[S])(implicit tx: S#Tx, phase: IPull.Phase): Seq[A] = {
      val b = Seq.newBuilder[A]
      b.sizeHint(elems)
      elems.foreach { in =>
        val v = pull.expr(in)
        b += v
      }
      b.result()
    }
  }

  private final class FindExpanded[S <: Sys[S], A](in: IExpr[S, Seq[A]], it: It.Expanded[S, A],
                                                   p: Ex[Boolean], tx0: S#Tx)
                                                  (implicit targets: ITargets[S])
    extends IExpr[S, Option[A]] {

    def value(implicit tx: S#Tx): Option[A] = ???

    def changed: IChangeEvent[S, Option[A]] = ???

    def dispose()(implicit tx: S#Tx): Unit = ???
  }

  final case class Find[A] private (in: Ex[Seq[A]], it: It[A], closure: Graph, p: Ex[Boolean])
    extends Ex[Option[A]] {

    type Repr[S <: Sys[S]] = IExpr[S, Option[A]]

    override def productPrefix: String = s"ExSeq$$Find" // serialization

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      val inEx = in.expand[S]
      val itEx = it.expand[S]
      import ctx.targets
      new FindExpanded[S, A](inEx, itEx, p, tx)
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
