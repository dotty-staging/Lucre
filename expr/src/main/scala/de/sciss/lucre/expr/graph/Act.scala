/*
 *  Act.scala
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

package de.sciss.lucre.expr.graph

import de.sciss.lucre.expr.impl.IActionImpl
import de.sciss.lucre.expr.{Context, IAction, IControl, IExpr}
import de.sciss.lucre.stm.Sys

import scala.language.{higherKinds, implicitConversions}
import scala.{Option => _Option}

object Act {
  def apply(xs: Act*): Act = SeqImpl(xs)

  private final class ExpandedSeq[S <: Sys[S]](xs: Seq[IAction[S]]) extends IActionImpl[S] {
    def executeAction()(implicit tx: S#Tx): Unit =
      xs.foreach(_.executeAction())
  }

  private final case class SeqImpl(xs: Seq[Act]) extends Act {
    type Repr[S <: Sys[S]] = IAction[S]

    override def productPrefix = "Act" // serialization

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] =
      new ExpandedSeq(xs.map(_.expand[S]))
  }

  final case class Link[A](source: Trig, sink: Act)
    extends Control {

    override def productPrefix = s"Act$$Link" // serialization

    type Repr[S <: Sys[S]] = IControl[S]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      val tr    = source.expand[S]
      val ac    = sink  .expand[S]
      ac.addSource(tr)
//      val peer  = tr.changed.react { implicit tx => _ => ac.executeAction() }
      IControl.empty // .wrap(peer)
    }
  }

  /** Treats an expression of actions as an action, simply
    * expanding its value every time it is called.
    */
  implicit def flatten(in: Ex[Act]): Act = Flatten(in)

  trait Option[S <: Sys[S]] extends IExpr[S, _Option[Act]] {
    def executeAction()(implicit tx: S#Tx): Boolean
  }

  implicit final class Ops (private val in: Ex[_Option[Act]]) extends AnyVal {
    def orNop: Act = OrNop(in)
  }

  private final class ExpandedFlatten[S <: Sys[S]](in: IExpr[S, Act])(implicit ctx: Context[S])
    extends IActionImpl[S] {

    def executeAction()(implicit tx: S#Tx): Unit = {
      val act = in.value
      val (actEx, d) = ctx.nested {
        act.expand[S]
      }
      actEx.executeAction()
      d.dispose()
    }
  }

  private final class ExpandedOrNop[S <: Sys[S]](in: IExpr[S, _Option[Act]])(implicit ctx: Context[S])
    extends IActionImpl[S] {

    def executeAction()(implicit tx: S#Tx): Unit = {
//      in match {
//        case o: Option[S] => o.executeAction()
//
//          // XXX TODO Huh, not cool. Perhaps we should return Act.Option directly from CanMap ?
//        case _ => throw new UnsupportedOperationException(s"Execute on a generic Ex[Option[Act]]")
////
      // println("in.value")
      val v = in.value
      // println(s"---> $v")
      v.foreach { act =>
        val (actEx, d) = ctx.nested {
          act.expand[S]
        }
        actEx.executeAction()
        d.dispose()
      }
    }
  }

  final case class OrNop(in: Ex[_Option[Act]]) extends Act {
    override def productPrefix = s"Act$$OrNop" // serialization

    type Repr[S <: Sys[S]] = IAction[S]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] =
      new ExpandedOrNop(in.expand[S])
  }

  final case class Flatten(in: Ex[Act]) extends Act {

    override def productPrefix = s"Act$$Flatten" // serialization

    type Repr[S <: Sys[S]] = IAction[S]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] =
      new ExpandedFlatten(in.expand[S])
  }

  private final class ExpandedNop[S <: Sys[S]]/*(implicit ctx: Context[S])*/
    extends IActionImpl[S] {

    def executeAction()(implicit tx: S#Tx): Unit = ()
  }

  final case class Nop() extends Act {

    override def productPrefix = s"Act$$Nop" // serialization

    type Repr[S <: Sys[S]] = IAction[S]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] =
      new ExpandedNop
  }
}
trait Act extends Lazy {
  type Repr[S <: Sys[S]] <: IAction[S]
}
