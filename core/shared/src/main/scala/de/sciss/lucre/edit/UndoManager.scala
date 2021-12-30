/*
 *  UndoManager.scala
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

package de.sciss.lucre
package edit

import de.sciss.lucre.Txn.peer
import de.sciss.lucre.edit.UndoManager.Update
import de.sciss.lucre.edit.impl.UndoManagerImpl

import scala.concurrent.stm.TxnLocal

object UndoManager {
  def dummy[T <: Txn[T]]: UndoManager[T] = UndoManagerImpl.dummy

  def apply[T <: Txn[T]](): UndoManager[T] =
    UndoManagerImpl()

  /** Runs a block of code under the given undo manager.
    * The counter part would be `suspend`.
    */
  def using[T <: Txn[T], A](m: UndoManager[T])(body: => A)(implicit tx: T): A = {
    val before = current.swap(m)
    try {
      body
    } finally {
      current() = before
    }
  }

  /** Runs a block of code ensuring that no undo manager is present.
    * The counter part would be `using`.
    *
    * The can be used if a method guarantees bypassing undo management,
    * but its implement relies on undo manager agnostic calls.
    */
  def suspend[T <: Txn[T], A](body: => A)(implicit tx: T): A =
    using[T, A](null)(body)

  /** Finds the currently (temporarily) set undo manager. */
  def find[T <: Txn[T]](implicit tx: T): Option[UndoManager[T]] =
    Option(current().asInstanceOf[UndoManager[T]])

  final case class Update[T <: Exec /*Txn*/[T]](m: UndoManager[T], undoName: Option[String], redoName: Option[String]) {
    def canUndo: Boolean = undoName.isDefined
    def canRedo: Boolean = redoName.isDefined
  }

  private val current = TxnLocal[UndoManager[_]]()

  final class CannotUndoException(message: String) extends RuntimeException(message)
  final class CannotRedoException(message: String) extends RuntimeException(message)
}
trait UndoManager[T <: Exec /*Txn*/[T]] extends Disposable[T] with Observable[T, UndoManager.Update[T]] {
  /** Add another edit to the history.
    * Unless merging is blocked, it tries to merge this edit
    * with the most recent edit. Afterwards,
    * the internal merge-block flag is cleared.
    */
  def addEdit(edit: UndoableEdit[T])(implicit tx: T): Unit

  /** Creates a compound capture if during the execution of `block` more than
    * one edit is added. If exactly one edit is added, it will be directly
    * added without a wrapping compound.
    *
    * '''Note:''' this wraps the call in `UndoManager.using`, so this manager
    * is found by agnostic code.
    */
  def capture[A](name: String)(block: => A)(implicit tx: T): A

//  def use[A](block: => A)(implicit tx: T): A =
//    UndoManager.using(this)(block)

  def use[A](block: => A)(implicit tx: T): A

  /** Disallow the merging of the next edit to be added.
    * This can be used to avoid merging edits if the editor
    * component was temporarily unfocused, for example.
    */
  def blockMerge()(implicit tx: T): Unit

  /** Whether there are undoable edits and thus `undo` and ` undoName` may be called. */
  def canUndo(implicit tx: T): Boolean

  /** Throws an exception if `!canUndo` */
  def undo()(implicit tx: T): Unit

  /** Throws an exception if `!canUndo` */
  def undoName(implicit tx: T): String

  /** Whether there are edits that can be redone, and thus whether `redo` and ` redoName` may be called. */
  def canRedo(implicit tx: T): Boolean

  /** Throws an exception if `!canRedo` */
  def redo()(implicit tx: T): Unit

  /** Throws an exception if `!canRedo` */
  def redoName(implicit tx: T): String

  /** Clears the history, removing all edits. Afterwards, `canUndo` and `canRedo` will return `false`. */
  def clear()(implicit tx: T): Unit

  /** A no-op that can be used to acknowledge the presence of the undo manager.
    *
    * This is useful to avoid warnings about an unused undo manager argument,
    * when the code proceeds to call undo manager agnostic implementations.
    */
  def ack(): Unit = ()

  def reactNow(fun: T => Update[T] => Unit)(implicit tx: T): Disposable[T] = {
    val state = Update(this,
      undoName = if (canUndo) Some(undoName) else None,
      redoName = if (canRedo) Some(redoName) else None
    )
    val obs   = react(fun)
    fun(tx)(state)
    obs
  }
}