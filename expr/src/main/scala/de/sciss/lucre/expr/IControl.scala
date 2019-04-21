/*
 *  IControl.scala
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

package de.sciss.lucre.expr

import de.sciss.lucre.stm.{Base, Disposable}

object IControl {
  def wrap[S <: Base[S]](peer: Disposable[S#Tx]): IControl[S] = new Wrap(peer)

  def empty[S <: Base[S]]: IControl[S] = new Empty

  private final class Wrap[S <: Base[S]](peer: Disposable[S#Tx]) extends IControl[S] {
    def init()(implicit tx: S#Tx): Unit = ()

    def dispose()(implicit tx: S#Tx): Unit = peer.dispose()
  }

  private final class Empty[S <: Base[S]] extends IControl[S] {
    def init()(implicit tx: S#Tx): Unit = ()

    def dispose()(implicit tx: S#Tx): Unit = ()
  }
}
trait IControl[S <: Base[S]] extends Disposable[S#Tx] {
  def init()(implicit tx: S#Tx): Unit
}