/*
 *  EditArtifact.scala
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
import de.sciss.lucre.artifact.Artifact
import de.sciss.lucre.artifact.Artifact.Child
import de.sciss.lucre.stm.UndoManager.{CannotRedoException, CannotUndoException}
import de.sciss.lucre.stm.impl.BasicUndoableEdit
import de.sciss.lucre.stm.{Sys, UndoManager}

object EditArtifact {
  def updateChild[S <: Sys[S]](a: Artifact.Modifiable[S], child: Child)(implicit tx: S#Tx): Unit =
    UndoManager.find[S].fold(
      updateChildDo   (a, child)
    ) { implicit undo =>
      updateChildUndo (a, child)
    }

  private def updateChildDo[S <: Sys[S]](a: Artifact.Modifiable[S], child: Child)(implicit tx: S#Tx): Unit =
    a.child = child

  def updateChildUndo[S <: Sys[S]](a: Artifact.Modifiable[S], child: Child)
                                  (implicit tx: S#Tx, undo: UndoManager[S]): Unit = {
    val edit = new UpdateChild[S](a, child, tx)
    undo.addEdit(edit)
  }

  private final class UpdateChild[S <: Sys[S]](a0: Artifact.Modifiable[S], child: Child, tx0: S#Tx)
    extends BasicUndoableEdit[S] {

    private[this] val aH    = tx0.newHandle(a0)
    private[this] val prev  = a0.child(tx0)

    private def invalidMessage = s"$name: value changed"

    private def cannotUndo(): Nothing =
      throw new CannotUndoException(invalidMessage)

    private def cannotRedo(): Nothing =
      throw new CannotRedoException(invalidMessage)

    protected def undoImpl()(implicit tx: S#Tx): Unit = {
      val a     = aH()
      // we are quite conservative: if conditions changes such that value
      // was no longer as expected, we refuse the operation
      if (a.child !== child) cannotUndo()
      a.child   = prev
    }

    protected def redoImpl()(implicit tx: S#Tx): Unit = {
      val a     = aH()
      // we are quite conservative: if conditions changes such that value
      // was no longer as expected, we refuse the operation
      if (a.child !== prev) cannotRedo()
      a.child   = child
    }

    def name: String = "Update Artifact"
  }
}
