/*
 *  Identifier.scala
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

package de.sciss.lucre.stm

import de.sciss.lucre.stm.impl.IdentifierSerializer
import de.sciss.serial
import de.sciss.serial.Serializer

object Identifier {
  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, S#ID] =
    anySer.asInstanceOf[IdentifierSerializer[S]]

  private val anySer = new IdentifierSerializer[NoSys]
}
trait Identifier[-Tx] extends Disposable[Tx] with serial.Writable