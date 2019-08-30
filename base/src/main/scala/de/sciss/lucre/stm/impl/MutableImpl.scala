/*
 *  MutableImpl.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2019 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
*
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.stm
package impl

import de.sciss.serial.DataOutput

trait MutableImpl[S <: Base[S]] extends Mutable[S#Id, S#Tx] {
  final def dispose()(implicit tx: S#Tx): Unit = {
    id.dispose()
    disposeData()
  }

  final def write(out: DataOutput): Unit = {
    id.write(out)
    writeData(out)
  }

  protected def disposeData()(implicit tx: S#Tx): Unit
  protected def writeData(out: DataOutput): Unit

  override def toString = s"${super.toString}$id"
}