/*
 *  UndoableEdit.scala
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

trait UndoableEdit[S <: Base[S]] extends Disposable[S#Tx] {
  def undo()(implicit tx: S#Tx): Unit
  def redo()(implicit tx: S#Tx): Unit

  /** Tries to merge this edit with a successive edit.
    *
    * @return   `Some` new edit containing both this and the successive edit, if possible,
    *           `None` if merging is not possible
    */
  def tryMerge(succ: UndoableEdit[S])(implicit tx: S#Tx): Option[UndoableEdit[S]]

  /** Whether this edit is destructive to the document model or not. */
  def significant: Boolean

  def name: String
}