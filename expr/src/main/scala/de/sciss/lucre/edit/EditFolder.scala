/*
 *  EditFolder.scala
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
import de.sciss.lucre.stm.UndoManager.{CannotRedoException, CannotUndoException}
import de.sciss.lucre.stm.impl.BasicUndoableEdit
import de.sciss.lucre.stm.{Folder, Obj, Sys, UndoManager}

object EditFolder {
  def append[S <: Sys[S]](parent: Folder[S], child: Obj[S])
                         (implicit tx: S#Tx): Unit =
    UndoManager.find[S].fold(
      appendDo  (parent, child)
    ) { implicit undo =>
      appendUndo(parent, child)
    }

  def appendUndo[S <: Sys[S]](parent: Folder[S], child: Obj[S])
                             (implicit tx: S#Tx, undo: UndoManager[S]): Unit = {
    val edit = new Append(parent, child, tx)
    undo.addEdit(edit)
  }

  def prepend[S <: Sys[S]](parent: Folder[S], child: Obj[S])
                         (implicit tx: S#Tx): Unit =
    UndoManager.find[S].fold(
      prependDo  (parent, child)
    ) { implicit undo =>
      prependUndo(parent, child)
    }

  def prependUndo[S <: Sys[S]](parent: Folder[S], child: Obj[S])
                             (implicit tx: S#Tx, undo: UndoManager[S]): Unit = {
    val edit = new Prepend(parent, child, tx)
    undo.addEdit(edit)
  }

  def removeHead[S <: Sys[S]](parent: Folder[S])(implicit tx: S#Tx): Obj[S] =
    UndoManager.find[S].fold(
      removeHeadDo  (parent)
    ) { implicit undo =>
      removeHeadUndo(parent)
    }

  def removeHeadUndo[S <: Sys[S]](parent: Folder[S])
                                 (implicit tx: S#Tx, undo: UndoManager[S]): Obj[S] = {
    val res   = parent.head
    val edit  = new RemoveHead(parent, tx)
    undo.addEdit(edit)
    res
  }

  def removeLast[S <: Sys[S]](parent: Folder[S])(implicit tx: S#Tx): Obj[S] =
    UndoManager.find[S].fold(
      removeLastDo  (parent)
    ) { implicit undo =>
      removeLastUndo(parent)
    }

  def removeLastUndo[S <: Sys[S]](parent: Folder[S])
                           (implicit tx: S#Tx, undo: UndoManager[S]): Obj[S] = {
    val res   = parent.head
    val edit  = new RemoveLast(parent, tx)
    undo.addEdit(edit)
    res
  }

  def removeAt[S <: Sys[S]](parent: Folder[S], index: Int)
                           (implicit tx: S#Tx): Obj[S] =
    UndoManager.find[S].fold(
      removeAtDo  (parent, index)
    ) { implicit undo =>
      removeAtUndo(parent, index)
    }

  def removeAtUndo[S <: Sys[S]](parent: Folder[S], index: Int)
                               (implicit tx: S#Tx, undo: UndoManager[S]): Obj[S] = {
    val res   = parent(index)
    val edit  = new RemoveAt(parent, index, tx)
    undo.addEdit(edit)
    res
  }

  // ---- private: append ----

  private def appendDo[S <: Sys[S]](parent: Folder[S], child: Obj[S])(implicit tx: S#Tx): Unit =
    parent.addLast(child)

  private final class Append[S <: Sys[S]](parent0: Folder[S], child0: Obj[S], tx0: S#Tx)
    extends BasicUndoableEdit[S] {

    private[this] val parentH = tx0.newHandle(parent0)
    private[this] val childH  = tx0.newHandle(child0 )

    appendDo(parent0, child0)(tx0)

    protected def undoImpl()(implicit tx: S#Tx): Unit = {
      val p   = parentH()
      val c   = childH ()
      val old = p.removeLast()
      if (old !== c) throw new CannotUndoException(s"$name: last element is not $c")
    }

    protected def redoImpl()(implicit tx: S#Tx): Unit =
      appendDo(parentH(), childH())

    def name: String = "Append to Folder"
  }

  // ---- private: prepend ----

  private def prependDo[S <: Sys[S]](parent: Folder[S], child: Obj[S])(implicit tx: S#Tx): Unit =
    parent.addHead(child)

  private final class Prepend[S <: Sys[S]](parent0: Folder[S], child0: Obj[S], tx0: S#Tx)
    extends BasicUndoableEdit[S] {

    private[this] val parentH = tx0.newHandle(parent0)
    private[this] val childH  = tx0.newHandle(child0 )

    prependDo(parent0, child0)(tx0)

    protected def undoImpl()(implicit tx: S#Tx): Unit = {
      val p   = parentH()
      val c   = childH ()
      val old = p.removeHead()
      if (old !== c) throw new CannotUndoException(s"$name: last element is not $c")
    }

    protected def redoImpl()(implicit tx: S#Tx): Unit =
      prependDo(parentH(), childH())

    def name: String = "Prepend to Folder"
  }

  // ---- private: removeHead ----

  private def removeHeadDo[S <: Sys[S]](parent: Folder[S])(implicit tx: S#Tx): Obj[S] =
    parent.removeHead()

  private final class RemoveHead[S <: Sys[S]](parent0: Folder[S], tx0: S#Tx)
    extends BasicUndoableEdit[S] {

    private[this] val parentH = tx0.newHandle(parent0)
    private[this] val childH  = tx0.newHandle(removeHeadDo(parent0)(tx0))
    private[this] val size    = parent0.size(tx0) // _after_ the removal!

    protected def undoImpl()(implicit tx: S#Tx): Unit = {
      val p   = parentH()
      val c   = childH ()
      val sz  = p.size
      if (sz !== size) throw new CannotUndoException(s"$name: contents has changed")
      p.addHead(c)
    }

    protected def redoImpl()(implicit tx: S#Tx): Unit = {
      val found = removeHeadDo(parentH())
      val c     = childH()
      if (found !== c) throw new CannotRedoException(s"$name: element at given index is not $c")
    }

    def name: String = "Remove from Folder"
  }

  // ---- private: removeLast ----

  private def removeLastDo[S <: Sys[S]](parent: Folder[S])(implicit tx: S#Tx): Obj[S] =
    parent.removeLast()

  private final class RemoveLast[S <: Sys[S]](parent0: Folder[S], tx0: S#Tx)
    extends BasicUndoableEdit[S] {

    private[this] val parentH = tx0.newHandle(parent0)
    private[this] val childH  = tx0.newHandle(removeLastDo(parent0)(tx0))
    private[this] val size    = parent0.size(tx0) // _after_ the removal!

    protected def undoImpl()(implicit tx: S#Tx): Unit = {
      val p   = parentH()
      val c   = childH ()
      val sz  = p.size
      if (sz !== size) throw new CannotUndoException(s"$name: contents has changed")
      p.addLast(c)
    }

    protected def redoImpl()(implicit tx: S#Tx): Unit = {
      val found = removeLastDo(parentH())
      val c     = childH()
      if (found !== c) throw new CannotRedoException(s"$name: element at given index is not $c")
    }

    def name: String = "Remove from Folder"
  }

  // ---- private: removeAt ----

  private def removeAtDo[S <: Sys[S]](parent: Folder[S], index: Int)(implicit tx: S#Tx): Obj[S] =
    parent.removeAt(index)

  private final class RemoveAt[S <: Sys[S]](parent0: Folder[S], index: Int, tx0: S#Tx)
    extends BasicUndoableEdit[S] {

    private[this] val parentH = tx0.newHandle(parent0)
    private[this] val childH  = tx0.newHandle(removeAtDo(parent0, index)(tx0))
    private[this] val size    = parent0.size(tx0) // _after_ the removal!

    protected def undoImpl()(implicit tx: S#Tx): Unit = {
      val p   = parentH()
      val c   = childH ()
      val sz  = p.size
      if (sz !== size) throw new CannotUndoException(s"$name: contents has changed")
      p.insert(index, c)
    }

    protected def redoImpl()(implicit tx: S#Tx): Unit = {
      val found = removeAtDo(parentH(), index)
      val c     = childH()
      if (found !== c) throw new CannotRedoException(s"$name: element at given index is not $c")
    }

    def name: String = "Remove from Folder"
  }
}
