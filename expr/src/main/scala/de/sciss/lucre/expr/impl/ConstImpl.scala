/*
 *  ConstImpl.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2015 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr
package impl

import de.sciss.lucre.stm.Sys
import de.sciss.serial.DataOutput

trait ConstImpl[S <: Sys[S], A] extends Expr.Const[S, A] {
  final def write(out: DataOutput): Unit = {
    out.writeInt(typeID)
    out.writeByte(3)
    writeData(out)
  }

  protected def writeData(out: DataOutput): Unit
}