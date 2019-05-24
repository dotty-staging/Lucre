/*
 *  ElemSerializer.scala
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

package de.sciss.lucre.stm
package impl

import de.sciss.serial
import de.sciss.serial.{DataInput, DataOutput}

trait ElemSerializer[S <: Sys[S], Repr <: Elem[S]]
  extends serial.Serializer[S#Tx, S#Acc, Repr] {

  protected def tpe: Elem.Type

  final def write(v: Repr, out: DataOutput): Unit = v.write(out)

  final def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Repr = {
    val tpe0 = in.readInt()
    if (tpe0 != tpe.typeId) sys.error(s"Type mismatch, expected ${tpe.typeId}, found $tpe0")
    tpe.readIdentifiedObj(in, access).asInstanceOf[Repr]
  }
}
