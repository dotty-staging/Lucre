/*
 *  BasicUndoableEdit.scala
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

package de.sciss.lucre.stm.impl

import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.stm.{Sys, UndoableEdit}

import scala.concurrent.stm.Ref

abstract class BasicUndoableEdit[S <: Sys[S]] extends UndoableEdit[S] {

  // ---- abstract ----

  protected def undoImpl()(implicit tx: S#Tx): Unit
  protected def redoImpl()(implicit tx: S#Tx): Unit

  // ---- impl ----

  private[this] val state = Ref(0)    // 0 'done', '1' undone, '2' disposed

  final def undo()(implicit tx: S#Tx): Unit = {
    require (state.swap(1) == 0)
    undoImpl()
  }

  final def redo()(implicit tx: S#Tx): Unit = {
    require (state.swap(0) == 1)
    redoImpl()
  }

  /** Returns `None`. Implementations may override. */
  def tryMerge(succ: UndoableEdit[S])(implicit tx: S#Tx): Option[UndoableEdit[S]] = None

  /** Returns `true`. Implementations may override. */
  def significant: Boolean = true

  /** Can be overridden if `super` is called. */
  def dispose()(implicit tx: S#Tx): Unit = {
    state() = 2
  }
}
