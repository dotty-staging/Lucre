/*
 *  EditExprVar.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2020 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.edit

import de.sciss.equal.Implicits._
import de.sciss.lucre.expr.{Expr, Type}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.UndoManager.{CannotRedoException, CannotUndoException}
import de.sciss.lucre.stm.impl.BasicUndoableEdit
import de.sciss.lucre.stm.{Sys, UndoManager}

object EditExprVar {
  def apply[S <: Sys[S], A, E[~ <: Sys[~]] <: Expr[~, A]](vr: E[S] with stm.Var[S#Tx, E[S]], value: E[S])
                                                         (implicit tx: S#Tx, tpe: Type.Expr[A, E]): Unit =
    UndoManager.find[S].fold(
      applyDo   [S, A, E](vr, value)
    ) { implicit undo =>
      applyUndo [S, A, E](vr, value)
    }

  def applyDo[S <: Sys[S], A, E[~ <: Sys[~]] <: Expr[~, A]](vr: E[S] with stm.Var[S#Tx, E[S]], value: E[S])
                                                           (implicit tx: S#Tx): Unit =
    vr() = value

  def applyUndo[S <: Sys[S], A, E[~ <: Sys[~]] <: Expr[~, A]](vr: E[S] with stm.Var[S#Tx, E[S]], value: E[S])
                                                             (implicit tx: S#Tx, tpe: Type.Expr[A, E],
                                                              undo: UndoManager[S]): Unit = {
    val edit = new Apply[S, A, E](vr, value, tx)
    undo.addEdit(edit)
  }

  private final class Apply[S <: Sys[S], A, E[~ <: Sys[~]] <: Expr[~, A]](vr0: E[S] with stm.Var[S#Tx, E[S]],
                                                                          value0: E[S], tx0: S#Tx)
                                                                         (implicit tpe: Type.Expr[A, E])
    extends BasicUndoableEdit[S] {

    import tpe.{serializer, varSerializer}

    private[this] val vrH       = tx0.newHandle(vr0)
    private[this] val valueH    = tx0.newHandle(value0)
    private[this] val prevH     = tx0.newHandle(vr0.swap(value0)(tx0))

    private def invalidMessage = s"$name: value changed"

    private def cannotUndo(): Nothing =
      throw new CannotUndoException(invalidMessage)

    private def cannotRedo(): Nothing =
      throw new CannotRedoException(invalidMessage)

    protected def undoImpl()(implicit tx: S#Tx): Unit = {
      val vr    = vrH()
      val v     = valueH()
      val prev  = prevH()
      val found = vr.swap(prev)
      // we are quite conservative: if conditions changes such that value
      // was no longer as expected, we refuse the operation
      if (found !== v) cannotUndo()
    }

    protected def redoImpl()(implicit tx: S#Tx): Unit = {
      val vr    = vrH()
      val v     = valueH()
      val prev  = prevH()
      val found = vr.swap(v)
      // we are quite conservative: if conditions changes such that value
      // was no longer as expected, we refuse the operation
      if (found !== prev) cannotRedo()
    }

    def name: String = "Update Variable"
  }
}
