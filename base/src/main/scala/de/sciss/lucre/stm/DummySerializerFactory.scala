/*
 *  DummySerializerFactory.scala
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

import de.sciss.serial.{DataInput, DataOutput, Serializer}

object DummySerializerFactory {
  def apply[S <: Base[S]]: DummySerializerFactory[S] = anySer.asInstanceOf[DummySerializerFactory[S]]

  private val anySer = new Impl[Plain, Nothing]

  private class Impl[S <: Base[S], A]
    extends Serializer[S#Tx, S#Acc, A] with DummySerializerFactory[S] {

    implicit def dummySerializer[A1]: Serializer[S#Tx, S#Acc, A1] =
      this.asInstanceOf[Serializer[S#Tx, S#Acc, A1]]

    def write(v: A, out: DataOutput): Unit = ()

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): A =
      sys.error("dummySerializer: Operation not supported")
  }
}

trait DummySerializerFactory[S <: Base[S]] {
  implicit def dummySerializer[A]: Serializer[S#Tx, S#Acc, A]
}