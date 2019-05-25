/*
 *  UndoManager.scala
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

package de.sciss.lucre.stm

import de.sciss.lucre.event.Observable
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.stm.impl.UndoManagerImpl

import scala.concurrent.stm.TxnLocal

object UndoManager {
  def apply[S <: Sys[S]](): UndoManager[S] =
    UndoManagerImpl()

  def using[S <: Sys[S], A](m: UndoManager[S])(body: => A)(implicit tx: S#Tx): A = {
    val before = current.swap(m)
    try {
      body
    } finally {
      current() = before
    }
  }

  def find[S <: Sys[S]](implicit tx: S#Tx): Option[UndoManager[S]] =
    Option(current().asInstanceOf[UndoManager[S]])

  final case class Update[S <: Sys[S]](m: UndoManager[S], undoName: Option[String], redoName: Option[String]) {
    def canUndo: Boolean = undoName.isDefined
    def canRedo: Boolean = redoName.isDefined
  }

  private val current = TxnLocal[UndoManager[_]]()

  final class CannotUndoException(message: String) extends RuntimeException(message) {
//    def this() = this(null)
  }
  final class CannotRedoException(message: String) extends RuntimeException(message) {
//    def this() = this(null)
  }
}
trait UndoManager[S <: Sys[S]] extends Disposable[S#Tx] with Observable[S#Tx, UndoManager.Update[S]] {
  /** Add another edit to the history.
    * Unless merging is blocked, it tries to merge this edit
    * with the most recent edit. Afterwards,
    * the internal merge-block flag is cleared.
    */
  def addEdit(edit: UndoableEdit[S])(implicit tx: S#Tx): Unit

  /** Creates a compound capture if during the execution of `block` more than
    * one edit is added. If exactly one edit is added, it will be directly
    * added without a wrapping compound.
    *
    * '''Note:''' it does not set the temporary global manager,
    * to do so, `UndoManager.using` must be called additionally!
    */
  def capture[A](name: String)(block: => A)(implicit tx: S#Tx): A

  /** Disallow the merging of the next edit to be added.
    * This can be used to avoid merging edits if the editor
    * component was temporarily unfocused, for example.
    */
  def blockMerge()(implicit tx: S#Tx): Unit

  /** Whether there are undoable edits and thus `undo` and ` undoName` may be called. */
  def canUndo(implicit tx: S#Tx): Boolean

  /** Throws an exception if `!canUndo` */
  def undo()(implicit tx: S#Tx): Unit

  /** Throws an exception if `!canUndo` */
  def undoName(implicit tx: S#Tx): String

  /** Whether there are edits that can be redone, and thus whether `redo` and ` redoName` may be called. */
  def canRedo(implicit tx: S#Tx): Boolean

  /** Throws an exception if `!canRedo` */
  def redo()(implicit tx: S#Tx): Unit

  /** Throws an exception if `!canRedo` */
  def redoName(implicit tx: S#Tx): String

  /** Clears the history, removing all edits. Afterwards, `canUndo` and `canRedo` will return `false`. */
  def clear()(implicit tx: S#Tx): Unit
}