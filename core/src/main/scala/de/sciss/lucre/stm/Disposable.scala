/*
 *  Disposable.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2017 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.stm

object Disposable {
  private[this] val emptyVal: Disposable[Any] = new Disposable[Any] {
    def dispose()(implicit tx: Any): Unit = ()
  }

  def empty[Tx]: Disposable[Tx] = emptyVal

  def seq[Tx](xs: Disposable[Tx]*): Disposable[Tx] = new Disposable[Tx] {
    def dispose()(implicit tx: Tx): Unit = xs.foreach(_.dispose())
  }
}
trait Disposable[-Tx] {
  def dispose()(implicit tx: Tx): Unit
}