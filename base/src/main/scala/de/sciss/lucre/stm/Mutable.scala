/*
 *  Mutable.scala
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

import de.sciss.serial.{DataOutput, Writable}

object Mutable {
  trait Impl[T <: Tx] extends Writable with Disposable[T]  {
    def id(implicit tx: T): tx.Id

    final def dispose()(implicit tx: T): Unit = {
      id(tx).dispose()(tx)
      disposeData()
    }

    final def write(out: DataOutput): Unit = {
      ??? // id.write(out)
      writeData(out)
    }

    protected def disposeData()(implicit tx: T): Unit
    protected def writeData(out: DataOutput): Unit

//    override def toString = s"${super.toString}$id"
  }
}
// trait Mutable[+ID, -Tx] extends Identifiable[ID] with Writable with Disposable[Tx]