/*
 *  Cursor.scala
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

package de.sciss.lucre.confluent

import de.sciss.lucre.confluent.impl.{CursorImpl => Impl}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Disposable
import de.sciss.serial
import de.sciss.serial.{Serializer, DataInput, Writable}

object Cursor {
  def apply[S <: Sys[S], D1 <: stm.DurableLike[D1]](init: S#Acc = Access.root[S])
                                                   (implicit tx: D1#Tx, system: S { type D = D1 }): Cursor[S, D1] =
    wrap(Data(init))

  def wrap[S <: Sys[S], D1 <: stm.DurableLike[D1]](data: Data[S, D1])
                                                   (implicit system: S { type D = D1 }): Cursor[S, D1] =
    Impl[S, D1](data)

  def read[S <: Sys[S], D1 <: stm.DurableLike[D1]](in: DataInput)
                                                  (implicit tx: D1#Tx, system: S { type D = D1 }): Cursor[S, D1] =
    Impl.read[S, D1](in)

  implicit def serializer[S <: Sys[S], D1 <: stm.DurableLike[D1]](
    implicit system: S { type D = D1 }): serial.Serializer[D1#Tx, D1#Acc, Cursor[S, D1]] = Impl.serializer[S, D1]

  object Data {
    def apply[S <: Sys[S], D <: stm.DurableLike[D]](init: S#Acc = Access.root[S])(implicit tx: D#Tx): Data[S, D] =
      Impl.newData[S, D](init)

    def read[S <: Sys[S], D <: stm.DurableLike[D]](in: DataInput)(implicit tx: D#Tx): Data[S, D] =
      Impl.newData()

    implicit def serializer[S <: Sys[S], D <: stm.DurableLike[D]]: Serializer[D#Tx, D#Acc, Data[S, D]] =
      Impl.dataSerializer[S, D]
  }
  trait Data[S <: Sys[S], D <: stm.DurableLike[D]] extends Disposable[D#Tx] with Writable {
    def id  : D#ID
    def path: D#Var[S#Acc]
  }
}
trait Cursor[S <: Sys[S], D <: stm.DurableLike[D]]
  extends stm.Cursor[S] with Disposable[D#Tx] with Writable {

  def data: Cursor.Data[S, D]

  def stepFrom[A](path: S#Acc, retroactive: Boolean = false)(fun: S#Tx => A): A
  def position(implicit tx: D#Tx): S#Acc
}