/*
 *  EditAttrMap.scala
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

package de.sciss.lucre.edit

import de.sciss.equal.Implicits._
import de.sciss.lucre.stm.Obj.AttrMap
import de.sciss.lucre.stm.UndoManager.{CannotRedoException, CannotUndoException}
import de.sciss.lucre.stm.impl.BasicUndoableEdit
import de.sciss.lucre.stm.{Obj, Sys, UndoManager}

object EditAttrMap {
  def put[S <: Sys[S]](map: AttrMap[S], key: String, value: Obj[S])(implicit tx: S#Tx): Unit =
    UndoManager.find[S].fold(
      putDo  (map, key, value)
    ) { implicit undo =>
      putUndo(map, key, value)
    }

  private def putDo[S <: Sys[S]](map: AttrMap[S], key: String, value: Obj[S])(implicit tx: S#Tx): Unit =
    map.put(key, value)

  def putUndo[S <: Sys[S]](map: AttrMap[S], key: String, value: Obj[S])(implicit tx: S#Tx,
                                                                        undo: UndoManager[S]): Unit = {
    val edit = new Put(map, key, value, tx)
    undo.addEdit(edit)
  }

  def remove[S <: Sys[S]](map: AttrMap[S], key: String)(implicit tx: S#Tx): Unit =
    UndoManager.find[S].fold(
      removeDo  (map, key)
    ) { implicit undo =>
      removeUndo(map, key)
    }

  private def removeDo[S <: Sys[S]](map: AttrMap[S], key: String)(implicit tx: S#Tx): Unit =
    map.remove(key)

  def removeUndo[S <: Sys[S]](map: AttrMap[S], key: String)(implicit tx: S#Tx,
                                                            undo: UndoManager[S]): Unit = {
    val edit = new Remove(map, key, tx)
    undo.addEdit(edit)
  }

  private abstract class PutRemove[S <: Sys[S]] extends BasicUndoableEdit[S] {
    private def invalidMessage = s"$name: value in map changed"

    final protected def cannotUndo(): Nothing =
      throw new CannotUndoException(invalidMessage)

    final protected def cannotRedo(): Nothing =
      throw new CannotRedoException(invalidMessage)
  }

  private final class Put[S <: Sys[S]](map0: AttrMap[S], key: String, value0: Obj[S], tx0: S#Tx)
    extends PutRemove[S] {

    private[this] val mapH      = tx0.newHandle(map0  )
    private[this] val valueH    = tx0.newHandle(value0)
    private[this] val prevHOpt  = map0.put(key, value0)(tx0).map(v => tx0.newHandle(v))

    protected def undoImpl()(implicit tx: S#Tx): Unit = {
      val m         = mapH  ()
      val v           = valueH()
      val foundOpt  = prevHOpt match {
        case Some(prevH)  => m.put(key, prevH())
        case None         => m.remove(key)
      }
      // we are quite conservative: if conditions changes such that value
      // was no longer at the key, we refuse the operation
      if (!foundOpt.contains(v)) cannotUndo()
    }

    protected def redoImpl()(implicit tx: S#Tx): Unit = {
      val m         = mapH()
      val v         = valueH()
      val foundOpt  = m.put(key, v)
      val prevOpt   = prevHOpt.map(_.apply())
      // we are quite conservative: if conditions changes such that previous value
      // is not the same as in the initial run, we refuse the operation
      if (foundOpt !== prevOpt) cannotRedo()
    }

    def name: String = s"Set Attribute $key"
  }

  private final class Remove[S <: Sys[S]](map0: AttrMap[S], key: String, tx0: S#Tx)
    extends PutRemove[S] {

    private[this] val mapH      = tx0.newHandle(map0  )
    private[this] val prevHOpt  = map0.remove(key)(tx0).map(v => tx0.newHandle(v))

    protected def undoImpl()(implicit tx: S#Tx): Unit = {
      val m         = mapH()
      val foundOpt  = prevHOpt match {
        case Some(prevH)  => m.put(key, prevH())
        case None         => None
      }
      // we are quite conservative: if conditions changes such that a value
      // was now present at the key, we refuse the operation
      if (foundOpt.isDefined) cannotUndo()
    }

    protected def redoImpl()(implicit tx: S#Tx): Unit = {
      val m         = mapH()
      val foundOpt  = m.remove(key)
      val prevOpt   = prevHOpt.map(_.apply())
      // we are quite conservative: if conditions changes such that previous value
      // is not the same as in the initial run, we refuse the operation
      if (foundOpt !== prevOpt) cannotRedo()
    }

    def name: String = s"Remove Attribute $key"
  }
}
