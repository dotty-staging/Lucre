/*
 *  Edit.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2019 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr.graph

import de.sciss.lucre.expr.impl.IActionImpl
import de.sciss.lucre.expr.{Context, IAction, IExpr}
import de.sciss.lucre.stm.{Sys, UndoManager}

object Edit {
  def apply(): Ex[Edit] = Impl()

//  private final class ApplyExpanded[S <: Sys[S]](e: IExpr[S, Edit], act: IAction[S])
//                                                (implicit protected val targets: ITargets[S])
//    extends IAction[S] with ITriggerConsumer[S, Unit] with IGenerator[S, Unit] with Caching {
//
//    def executeAction()(implicit tx: S#Tx): Unit =
//      trigReceived() // .foreach(fire) --- we don't need to fire, there is nobody listening;
//
//    protected def trigReceived()(implicit tx: S#Tx): Option[Unit] = {
//
//      Trig.Some
//    }
//
//    def changed: IEvent[S, Unit] = this
//  }

  private final class ApplyExpanded[S <: Sys[S]](e: IExpr[S, Edit], act: IAction[S])
                                                (implicit ctx: Context[S])
    extends IActionImpl[S] {

    def executeAction()(implicit tx: S#Tx): Unit = {
      val undo = e.value.peer[S]
      UndoManager.using(undo) {
        act.executeAction()
      }
    }
  }

  final case class Apply(e: Ex[Edit], act: Act) extends Act {
    override def productPrefix: String = s"Edit$$Apply"  // serialization

    type Repr[S <: Sys[S]] = IAction[S]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] =
      new ApplyExpanded(e.expand[S], act.expand[S])
  }

  final case class Named(e: Ex[Edit], name: Ex[String], act: Act*) extends Act {
    override def productPrefix: String = s"Edit$$Named"  // serialization

    type Repr[S <: Sys[S]] = IAction[S]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = ???
  }

  private final class Expanded[In <: Sys[In]](in: UndoManager[In], system: In) extends Edit {
    private[graph] def peer[S <: Sys[S]](implicit tx: S#Tx): UndoManager[S] = {
      require (tx.system == system)
      in.asInstanceOf[UndoManager[S]]
    }
  }

  private final case class Impl() extends Ex[Edit] {
    override def productPrefix: String = "Edit"  // serialization

    type Repr[S <: Sys[S]] = IExpr[S, Edit]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] =
      new Const.Expanded(new Expanded[S](ctx.undoManager, tx.system))
  }

  implicit final class Ops(private val x: Ex[Edit]) extends AnyVal {
    def apply(name: Ex[String])(act: Act*): Act = Named(x, name, act: _*)
    def apply(act: Act): Act = Apply(x, act)
  }
}
trait Edit {
  private[graph] def peer[S <: Sys[S]](implicit tx: S#Tx): UndoManager[S]
}