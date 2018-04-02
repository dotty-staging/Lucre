/*
 *  Disposable.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2018 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.stm

//import scala.language.implicitConversions

object Disposable {
//  implicit def FromComp[S <: Base](c: Comp[S]): Disposable[c.S.Tx] = new Disposable[c.S.Tx] {
//    def dispose()(implicit tx: c.S.Tx): Unit = c.dispose()
//  }

  private[this] val emptyVal: Disposable[Any] = new Disposable[Any] {
    def dispose()(implicit tx: Any): Unit = ()
  }

  def empty[Tx]: Disposable[Tx] = emptyVal

  def seq[Tx](xs: Disposable[Tx]*): Disposable[Tx] = new Disposable[Tx] {
    def dispose()(implicit tx: Tx): Unit = xs.foreach(_.dispose())
  }
}
trait Disposable[-Tx] extends Any {
  def dispose()(implicit tx: Tx): Unit
}

//abstract class DispComp[S <: Base](val S: S) extends {
//} with Disposable[S.Tx]
