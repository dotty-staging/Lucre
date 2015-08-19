/*
 *  Mutable.scala
 *  (Lucre)
 *
 *  Copyright (c) 2011-2015 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.stm

import de.sciss.serial.Writable

//object Mutable {
//  trait Impl[S <: Sys[S]] extends Mutable[S#ID, S#Tx] {
//    final def dispose()(implicit tx: S#Tx): Unit = {
//      id.dispose()
//      disposeData()
//    }
//
//    final def write(out: DataOutput): Unit = {
//      id.write(out)
//      writeData(out)
//    }
//
//    protected def disposeData()(implicit tx: S#Tx): Unit
//    protected def writeData(out: DataOutput): Unit
//
//    override def toString = s"${super.toString}$id"
//  }
//}
trait Mutable[+ID, -Tx] extends Identifiable[ID] with Writable with Disposable[Tx]