/*
 *  CursorImpl.scala
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

package de.sciss.lucre.confluent
package impl

import de.sciss.lucre.confluent.Cursor.Data
import de.sciss.lucre.stm.Txn
import de.sciss.lucre.{confluent, stm}
import de.sciss.serial
import de.sciss.serial.{DataInput, DataOutput, Serializer}

object CursorImpl {
  private final val COOKIE  = 0x4375  // "Cu"

  implicit def serializer[S <: Sys[S], D1 <: stm.DurableLike[D1]](
    implicit system: S { type D = D1 }): serial.Serializer[D1#Tx, D1#Acc, Cursor[S, D1]] = new Ser[S, D1]

  private final class Ser[S <: Sys[S], D1 <: stm.DurableLike[D1]](implicit system: S { type D = D1 })
    extends serial.Serializer[D1#Tx, D1#Acc, Cursor[S, D1]] {

    def write(v: Cursor[S, D1], out: DataOutput): Unit = v.write(out)

    def read(in: DataInput, access: Unit)(implicit tx: D1#Tx): Cursor[S, D1] = CursorImpl.read[S, D1](in)
  }

  private trait NoSys extends Sys[NoSys] { type D = stm.Durable }

  /* implicit */ def pathSerializer[S <: Sys[S], D <: stm.Sys[D]]: Serializer[D#Tx, D#Acc, S#Acc] =
    anyPathSer.asInstanceOf[PathSer[S, D]]

  private final val anyPathSer = new PathSer[NoSys, NoSys#D]

  private final class PathSer[S <: Sys[S], D1 <: stm.Sys[D1]] // (implicit system: S { type D = D1 })
    extends Serializer[D1#Tx, D1#Acc, S#Acc] {

    def write(v: S#Acc, out: DataOutput): Unit = v.write(out)

    def read(in: DataInput, access: D1#Acc)(implicit tx: D1#Tx): S#Acc =
      confluent.Access.read(in) // system.readPath(in)
  }

  def newData[S <: Sys[S], D <: stm.Sys[D]](init: S#Acc = Access.root[S])(implicit tx: D#Tx): Data[S, D] = {
    val id    = tx.newId()
    val path  = tx.newVar(id, init)(pathSerializer[S, D])
    new DataImpl[S, D](id, path)
  }

  def dataSerializer[S <: Sys[S], D <: stm.Sys[D]]: Serializer[D#Tx, D#Acc, Data[S, D]] =
    anyDataSer.asInstanceOf[DataSer[S, D]]

  private final val anyDataSer = new DataSer[NoSys, NoSys#D]

  private final class DataSer[S <: Sys[S], D <: stm.Sys[D]]
    extends Serializer[D#Tx, D#Acc, Data[S, D]] {

    def write(d: Data[S, D], out: DataOutput): Unit = d.write(out)

    def read(in: DataInput, access: D#Acc)(implicit tx: D#Tx): Data[S, D] = readData[S, D](in, access)
  }

  def readData[S <: Sys[S], D <: stm.Sys[D]](in: DataInput, access: D#Acc)(implicit tx: D#Tx): Data[S, D] = {
    val cookie  = in.readShort()
    if (cookie != COOKIE) throw new IllegalStateException(s"Unexpected cookie $cookie (should be $COOKIE)")
    val id      = tx.readId(in, access) // implicitly[Serializer[D#Tx, D#Acc, D#Id]].read(in)
    val path    = tx.readVar[S#Acc](id, in)(pathSerializer[S, D])
    new DataImpl[S, D](id, path)
  }

  private final class DataImpl[S <: Sys[S], D <: stm.Sys[D]](val id: D#Id, val path: D#Var[S#Acc])
    extends Data[S, D] {

    def write(out: DataOutput): Unit = {
      out.writeShort(COOKIE)
      id  .write(out)
      path.write(out)
    }

    def dispose()(implicit tx: D#Tx): Unit = {
      path.dispose()
      id  .dispose()
    }
  }

  def apply[S <: Sys[S], D1 <: stm.DurableLike[D1]](data: Data[S, D1])
                                                  (implicit system: S { type D = D1 }): Cursor[S, D1] =
    new Impl[S, D1](data)

  def read[S <: Sys[S], D1 <: stm.DurableLike[D1]](in: DataInput)
                                                  (implicit tx: D1#Tx, system: S { type D = D1 }): Cursor[S, D1] = {
    val data = readData[S, D1](in, ())
    Cursor.wrap[S, D1](data)
  }

  private final class Impl[S <: Sys[S], D1 <: stm.DurableLike[D1]](val data: Data[S, D1])
                                                          (implicit system: S { type D = D1 })
    extends Cursor[S, D1] with Cache[S#Tx] {

    override def toString = s"Cursor${data.id}"

    private def topLevelAtomic[A](fun: D1#Tx => A): A = Txn.atomic { itx =>
      val dtx = system.durable.wrap(itx)
      fun(dtx)
    }

    def step[A](fun: S#Tx => A): A = stepTag(0L)(fun)

    def stepTag[A](systemTimeNanos: Long)(fun: S#Tx => A): A = {
      topLevelAtomic { implicit dtx =>
        val inputAccess = data.path()
        performStep(inputAccess, systemTimeNanos = systemTimeNanos, retroactive = false, dtx = dtx, fun = fun)
      }
    }

    def stepFrom[A](inputAccess: S#Acc, retroactive: Boolean, systemTimeNanos: Long)(fun: S#Tx => A): A = {
      topLevelAtomic { implicit dtx =>
        data.path() = inputAccess
        performStep(inputAccess, systemTimeNanos = systemTimeNanos, retroactive = retroactive, dtx = dtx, fun = fun)
      }
    }

    private def performStep[A](inputAccess: S#Acc, retroactive: Boolean, systemTimeNanos: Long,
                               dtx: D1#Tx, fun: S#Tx => A): A = {
      val tx = system.createTxn(dtx = dtx, inputAccess = inputAccess, retroactive = retroactive,
        cursorCache = this, systemTimeNanos = systemTimeNanos)
      logCursor(s"${data.id} step. input path = $inputAccess")
      fun(tx)
    }

    def flushCache(term: Long)(implicit tx: S#Tx): Unit = {
      implicit val dtx: D1#Tx = system.durableTx(tx)
      val newPath = tx.inputAccess.addTerm(term)
      data.path() = newPath
      logCursor(s"${data.id} flush path = $newPath")
    }

    def position(implicit tx: S #Tx): S#Acc = position(system.durableTx(tx))
    def position(implicit tx: D1#Tx): S#Acc = data.path()

    def dispose()(implicit tx: D1#Tx): Unit = {
      data.dispose()
//      id  .dispose()
//      path.dispose()
      logCursor(s"${data.id} dispose")
    }

    def write(out: DataOutput): Unit = data.write(out)
  }
}