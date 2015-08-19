package de.sciss.lucre.expr
package impl

import de.sciss.lucre.stm.Sys
import de.sciss.serial.DataOutput

trait ConstImpl[S <: Sys[S], A] extends Expr.Const[S, A] {
  final def write(out: DataOutput): Unit = {
    out.writeByte(3)
    writeData(out)
  }

  protected def writeData(out: DataOutput): Unit
}