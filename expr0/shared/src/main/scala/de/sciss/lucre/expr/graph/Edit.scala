/*
 *  Edit.scala
 *  (Lucre 4)
 *
 *  Copyright (c) 2009-2022 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr.graph

import de.sciss.lucre.edit.UndoManager
import de.sciss.lucre.expr.ExElem.{ProductReader, RefMapIn}
import de.sciss.lucre.expr.impl.IActionImpl
import de.sciss.lucre.expr.{Context, IAction}
import de.sciss.lucre.{IExpr, Sys, Txn}

object Edit extends ProductReader[Ex[Edit]] {
  def apply(): Ex[Edit] = Impl()

  private final class ApplyExpanded[T <: Txn[T]](e: IExpr[T, Edit], act: IAction[T])
    extends IActionImpl[T] {

    def executeAction()(implicit tx: T): Unit = {
      val undo = e.value.peer[T]
      UndoManager.using(undo) {
        act.executeAction()
      }
    }
  }

  private[lucre] object Empty extends Edit {
    private[lucre] def peer[T <: Txn[T]](implicit tx: T): UndoManager[T] = UndoManager.dummy
  }

  object Apply extends ProductReader[Apply] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Apply = {
      require (arity == 2 && adj == 0)
      val _e    = in.readEx[Edit]()
      val _act  = in.readAct()
      new Apply(_e, _act)
    }
  }
  final case class Apply(e: Ex[Edit], act: Act) extends Act {
    override def productPrefix: String = s"Edit$$Apply"  // serialization

    type Repr[T <: Txn[T]] = IAction[T]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
      new ApplyExpanded(e.expand[T], act.expand[T])
  }

  private final class NamedExpanded[T <: Txn[T]](e: IExpr[T, Edit], name: IExpr[T, String], xs: Seq[IAction[T]])
    extends IActionImpl[T] {

    def executeAction()(implicit tx: T): Unit = {
      val undo  = e.value.peer[T]
      val nameV = name.value
      undo.capture(nameV) {
        xs.foreach(_.executeAction())
      }
    }
  }

  object Named extends ProductReader[Named] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Named = {
      require (arity == 3 && adj == 0)
      val _e      = in.readEx[Edit]()
      val _name   = in.readEx[String]()
      val _act    = in.readVec(in.readAct())
      new Named(_e, _name, _act: _*)
    }
  }
  final case class Named(e: Ex[Edit], name: Ex[String], act: Act*) extends Act {
    override def productPrefix: String = s"Edit$$Named"  // serialization

    type Repr[T <: Txn[T]] = IAction[T]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
      new NamedExpanded(e.expand[T], name.expand[T], act.map(_.expand[T]))
  }

  private final class Expanded[In <: Txn[In]](in: UndoManager[In], system: Sys) extends Edit {
    private[lucre] def peer[T <: Txn[T]](implicit tx: T): UndoManager[T] = {
      require (tx.system == system)
      in.asInstanceOf[UndoManager[T]]
    }
  }

  private final case class Impl() extends Ex[Edit] {
    override def productPrefix: String = "Edit"  // serialization

    type Repr[T <: Txn[T]] = IExpr[T, Edit]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
      new Const.Expanded(new Expanded[T](ctx.undoManager, tx.system))
  }

  override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Ex[Edit] = {
    require (arity == 0 && adj == 0)
    Edit()
  }

  implicit final class Ops(private val x: Ex[Edit]) extends AnyVal {
    def apply(name: Ex[String])(act: Act*): Act = Named(x, name, act: _*)
    def apply(act: Act): Act = Apply(x, act)
  }
}
trait Edit {
  private[lucre] def peer[T <: Txn[T]](implicit tx: T): UndoManager[T]
}