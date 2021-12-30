/*
 *  EditFolder.scala
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

import de.sciss.equal.Implicits._
import de.sciss.lucre.Txn.{peer => txPeer}
import de.sciss.lucre.edit.UndoManager.{CannotRedoException, CannotUndoException}
import de.sciss.lucre.edit.impl.BasicUndoableEdit

import scala.concurrent.stm.Ref

object EditFolder {
  def append[T <: Txn[T]](parent: Folder[T], child: Obj[T])
                         (implicit tx: T): Unit =
    UndoManager.find[T].fold(
      appendDo  (parent, child)
    ) { implicit undo =>
      appendUndo(parent, child)
    }

  def appendUndo[T <: Txn[T]](parent: Folder[T], child: Obj[T])
                             (implicit tx: T, undo: UndoManager[T]): Unit = {
    val edit = new Append(parent, child, tx)
    undo.addEdit(edit)
  }

  def prepend[T <: Txn[T]](parent: Folder[T], child: Obj[T])
                         (implicit tx: T): Unit =
    UndoManager.find[T].fold(
      prependDo  (parent, child)
    ) { implicit undo =>
      prependUndo(parent, child)
    }

  def prependUndo[T <: Txn[T]](parent: Folder[T], child: Obj[T])
                             (implicit tx: T, undo: UndoManager[T]): Unit = {
    val edit = new Prepend(parent, child, tx)
    undo.addEdit(edit)
  }

  def insert[T <: Txn[T]](parent: Folder[T], index: Int, child: Obj[T])
                         (implicit tx: T): Unit =
    UndoManager.find[T].fold(
      insertDo  (parent, index, child)
    ) { implicit undo =>
      insertUndo(parent, index, child)
    }

  def insertUndo[T <: Txn[T]](parent: Folder[T], index: Int, child: Obj[T])
                             (implicit tx: T, undo: UndoManager[T]): Unit = {
    val edit = new Insert(parent, index, child, tx)
    undo.addEdit(edit)
  }

  def removeHead[T <: Txn[T]](parent: Folder[T])(implicit tx: T): Obj[T] =
    UndoManager.find[T].fold(
      removeHeadDo  (parent)
    ) { implicit undo =>
      removeHeadUndo(parent)
    }

  def removeHeadUndo[T <: Txn[T]](parent: Folder[T])
                                 (implicit tx: T, undo: UndoManager[T]): Obj[T] = {
    val res   = parent.head
    val edit  = new RemoveHead(parent, tx)
    undo.addEdit(edit)
    res
  }

  def removeLast[T <: Txn[T]](parent: Folder[T])(implicit tx: T): Obj[T] =
    UndoManager.find[T].fold(
      removeLastDo  (parent)
    ) { implicit undo =>
      removeLastUndo(parent)
    }

  def removeLastUndo[T <: Txn[T]](parent: Folder[T])
                           (implicit tx: T, undo: UndoManager[T]): Obj[T] = {
    val res   = parent.head
    val edit  = new RemoveLast(parent, tx)
    undo.addEdit(edit)
    res
  }

  def removeAt[T <: Txn[T]](parent: Folder[T], index: Int)
                           (implicit tx: T): Obj[T] =
    UndoManager.find[T].fold(
      removeAtDo  (parent, index)
    ) { implicit undo =>
      removeAtUndo(parent, index)
    }

  def removeAtUndo[T <: Txn[T]](parent: Folder[T], index: Int)
                               (implicit tx: T, undo: UndoManager[T]): Obj[T] = {
    val res   = parent(index)
    val edit  = new RemoveAt(parent, index, tx)
    undo.addEdit(edit)
    res
  }

  def remove[T <: Txn[T]](parent: Folder[T], child: Obj[T])
                           (implicit tx: T): Boolean =
    UndoManager.find[T].fold(
      removeDo  (parent, child) >= 0
    ) { implicit undo =>
      removeUndo(parent, child)
    }

  def removeUndo[T <: Txn[T]](parent: Folder[T], child: Obj[T])
                               (implicit tx: T, undo: UndoManager[T]): Boolean = {
    val edit  = new Remove(parent, child, tx)
    val res   = edit.removed
    if (res) undo.addEdit(edit)
    res
  }

  def clear[T <: Txn[T]](parent: Folder[T])(implicit tx: T): Unit =
    UndoManager.find[T].fold(
      clearDo  (parent)
    ) { implicit undo =>
      clearUndo(parent)
    }

  def clearUndo[T <: Txn[T]](parent: Folder[T])
                            (implicit tx: T, undo: UndoManager[T]): Unit = {
    val edit = new Clear(parent, tx)
    undo.addEdit(edit)
  }

  // ---- private: append ----

  private def appendDo[T <: Txn[T]](parent: Folder[T], child: Obj[T])(implicit tx: T): Unit =
    parent.addLast(child)

  private final class Append[T <: Txn[T]](parent0: Folder[T], child0: Obj[T], tx0: T)
    extends BasicUndoableEdit[T] {

    private[this] val parentH = tx0.newHandle(parent0)
    private[this] val childH  = tx0.newHandle(child0 )

    appendDo(parent0, child0)(tx0)

    protected def undoImpl()(implicit tx: T): Unit = {
      val p   = parentH()
      val c   = childH ()
      val old = p.removeLast()
      if (old !== c) throw new CannotUndoException(s"$name: last element is not $c")
    }

    protected def redoImpl()(implicit tx: T): Unit =
      appendDo(parentH(), childH())

    def name: String = "Append to Folder"
  }

  // ---- private: prepend ----

  private def prependDo[T <: Txn[T]](parent: Folder[T], child: Obj[T])(implicit tx: T): Unit =
    parent.addHead(child)

  private final class Prepend[T <: Txn[T]](parent0: Folder[T], child0: Obj[T], tx0: T)
    extends BasicUndoableEdit[T] {

    private[this] val parentH = tx0.newHandle(parent0)
    private[this] val childH  = tx0.newHandle(child0 )

    prependDo(parent0, child0)(tx0)

    protected def undoImpl()(implicit tx: T): Unit = {
      val p   = parentH()
      val c   = childH ()
      val old = p.removeHead()
      if (old !== c) throw new CannotUndoException(s"$name: last element is not $c")
    }

    protected def redoImpl()(implicit tx: T): Unit =
      prependDo(parentH(), childH())

    def name: String = "Prepend to Folder"
  }

  // ---- private: insert ----

  private def insertDo[T <: Txn[T]](parent: Folder[T], index: Int, child: Obj[T])(implicit tx: T): Unit =
    parent.insert(index, child)

  private final class Insert[T <: Txn[T]](parent0: Folder[T], index: Int, child0: Obj[T], tx0: T)
    extends BasicUndoableEdit[T] {

    private[this] val parentH = tx0.newHandle(parent0)
    private[this] val childH  = tx0.newHandle(child0 )

    insertDo(parent0, index, child0)(tx0)

    protected def undoImpl()(implicit tx: T): Unit = {
      val p   = parentH()
      val c   = childH ()
      val old = p.removeAt(index)
      if (old !== c) throw new CannotUndoException(s"$name: last element is not $c")
    }

    protected def redoImpl()(implicit tx: T): Unit =
      insertDo(parentH(), index, childH())

    def name: String = "Insert in Folder"
  }

  // ---- private: removeHead ----

  private def removeHeadDo[T <: Txn[T]](parent: Folder[T])(implicit tx: T): Obj[T] =
    parent.removeHead()

  private final class RemoveHead[T <: Txn[T]](parent0: Folder[T], tx0: T)
    extends BasicUndoableEdit[T] {

    private[this] val parentH = tx0.newHandle(parent0)
    private[this] val childH  = tx0.newHandle(removeHeadDo(parent0)(tx0))
    private[this] val size    = parent0.size(tx0) // _after_ the removal!

    protected def undoImpl()(implicit tx: T): Unit = {
      val p   = parentH()
      val c   = childH ()
      val sz  = p.size
      if (sz !== size) throw new CannotUndoException(s"$name: contents has changed")
      p.addHead(c)
    }

    protected def redoImpl()(implicit tx: T): Unit = {
      val found = removeHeadDo(parentH())
      val c     = childH()
      if (found !== c) throw new CannotRedoException(s"$name: element at given index is not $c")
    }

    def name: String = "Remove from Folder"
  }

  // ---- private: removeLast ----

  private def removeLastDo[T <: Txn[T]](parent: Folder[T])(implicit tx: T): Obj[T] =
    parent.removeLast()

  private final class RemoveLast[T <: Txn[T]](parent0: Folder[T], tx0: T)
    extends BasicUndoableEdit[T] {

    private[this] val parentH = tx0.newHandle(parent0)
    private[this] val childH  = tx0.newHandle(removeLastDo(parent0)(tx0))
    private[this] val size    = parent0.size(tx0) // _after_ the removal!

    protected def undoImpl()(implicit tx: T): Unit = {
      val p   = parentH()
      val c   = childH ()
      val sz  = p.size
      if (sz !== size) throw new CannotUndoException(s"$name: contents has changed")
      p.addLast(c)
    }

    protected def redoImpl()(implicit tx: T): Unit = {
      val found = removeLastDo(parentH())
      val c     = childH()
      if (found !== c) throw new CannotRedoException(s"$name: element at given index is not $c")
    }

    def name: String = "Remove from Folder"
  }

  // ---- private: removeAt ----

  def removeAtDo[T <: Txn[T]](parent: Folder[T], index: Int)(implicit tx: T): Obj[T] =
    parent.removeAt(index)

  private final class RemoveAt[T <: Txn[T]](parent0: Folder[T], index: Int, tx0: T)
    extends BasicUndoableEdit[T] {

    private[this] val parentH = tx0.newHandle(parent0)
    private[this] val childH  = tx0.newHandle(removeAtDo(parent0, index)(tx0))
    private[this] val size    = parent0.size(tx0) // _after_ the removal!

    protected def undoImpl()(implicit tx: T): Unit = {
      val p   = parentH()
      val c   = childH ()
      val sz  = p.size
      if (sz !== size) throw new CannotUndoException(s"$name: contents has changed")
      p.insert(index, c)
    }

    protected def redoImpl()(implicit tx: T): Unit = {
      val found = removeAtDo(parentH(), index)
      val c     = childH()
      if (found !== c) throw new CannotRedoException(s"$name: element at given index is not $c")
    }

    def name: String = "Remove from Folder"
  }

  // ---- private: remove ----

  private def removeDo[T <: Txn[T]](parent: Folder[T], child: Obj[T])(implicit tx: T): Int = {
    val idx = parent.indexOf(child)
    if (idx >= 0) parent.removeAt(idx)
    idx
  }

  private final class Remove[T <: Txn[T]](parent0: Folder[T], child0: Obj[T], tx0: T)
    extends BasicUndoableEdit[T] {

    private[this] val parentH   = tx0.newHandle(parent0)
    private[this] val childH    = tx0.newHandle(child0)
    private[this] val indexH    = Ref(removeDo(parent0, child0)(tx0))

    def removed(implicit tx: T): Boolean = indexH() >= 0

    protected def undoImpl()(implicit tx: T): Unit = {
      val p     = parentH ()
      val c     = childH  ()
      val index = indexH  ()
      val sz    = p.size
      if (sz <= index) throw new CannotUndoException(s"$name: contents has changed")
      p.insert(index, c)
    }

    protected def redoImpl()(implicit tx: T): Unit = {
      val found = removeDo(parentH(), childH())
      if (found < 0) throw new CannotRedoException(s"$name: contents has changed")
      indexH()    = found
    }

    def name: String = "Remove from Folder"
  }

  // ---- private: clear ----

  private def clearDo[T <: Txn[T]](parent: Folder[T])(implicit tx: T): Unit =
    parent.clear()

  private final class Clear[T <: Txn[T]](parent0: Folder[T], tx0: T)
    extends BasicUndoableEdit[T] {

    private[this] val parentH   = tx0.newHandle(parent0)
    private[this] val childrenH = {
      val res = parent0.iterator(tx0).map(tx0.newHandle(_)).toList
      clearDo(parent0)(tx0)
      res
    }

    protected def undoImpl()(implicit tx: T): Unit = {
      val p = parentH()
      if (p.nonEmpty) throw new CannotUndoException(s"$name: contents has changed")
      childrenH.foreach { childH =>
        val c = childH()
        p.addLast(c)
      }
    }

    protected def redoImpl()(implicit tx: T): Unit = {
      val p = parentH()
      clearDo(p)
    }

    def name: String = "Clear Folder"
  }
}
