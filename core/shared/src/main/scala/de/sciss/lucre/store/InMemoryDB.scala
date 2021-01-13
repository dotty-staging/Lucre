/*
 *  InMemoryDB.scala
 *  (Lucre 4)
 *
 *  Copyright (c) 2009-2021 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre
package store

import de.sciss.lucre.Txn.peer
import de.sciss.serial.{ByteArrayStream, DataInput, DataOutput}

import java.util.concurrent.ConcurrentLinkedQueue
import scala.concurrent.stm.TMap
import scala.language.implicitConversions
import scala.util.hashing.MurmurHash3

object InMemoryDB {
  trait Factory extends DataStore.Factory {
    override def open(name: String, overwrite: Boolean = false): InMemoryDB
  }

  def factory(): Factory = new FactoryImpl

  def apply(name: String = "data"): InMemoryDB =
    factory().open(name)

  private final val BIN_COOKIE = 0x6c75632d69646200L   // "luc-idb\u0000"

  private type Value = Array[Byte]

  def fromByteArray(arr: Array[Byte]): InMemoryDB = {
    val in      = DataInput(arr)
    val cookie  = in.readLong()
    if (cookie != BIN_COOKIE) {
      sys.error(s"Unexpected cookie 0x${cookie.toHexString} is not 0x${BIN_COOKIE.toHexString}")
    }
    val name    = in.readUTF()
    val num     = in.readInt()
    val m       = TMap.newBuilder[Key, Value]
    var i = 0
    while (i < num) {
      val keySize = in.readInt()
      val keyArr  = new Array[Byte](keySize)
      in.readFully(keyArr)
      val key     = new Key(keyArr)
      val valueSize = in.readInt()
      val value   = new Array[Byte](valueSize)
      in.readFully(value)
      m += ((key, value))
      i += 1
    }
    new Impl(name, m.result())
  }

  private final class FactoryImpl extends Factory {
    def open(name: String, overwrite: Boolean): InMemoryDB =
      new Impl(name, TMap.empty)
  }

  private final class Key(val arr: Array[Byte]) {
    private[this] final val hash = MurmurHash3.bytesHash(arr)

    override def hashCode(): Int = hash

    override def equals(that: Any): Boolean = that match {
      case thatKey: Key =>
        this.hashCode() == thatKey.hashCode() && this.arr.sameElements(thatKey.arr)

      case _ => false
    }
  }

  private[this] final class Impl(name: String, map: TMap[Key, Value])
    extends InMemoryDB {

    override def toString: String = s"InMemoryDB($name)"

    override def toByteArray(implicit tx: TxnLike): Array[Byte] = {
      val out = DataOutput()
      out.writeLong(BIN_COOKIE)
      out.writeUTF(name)
      val m = map
      out.writeInt(m.size)
      m.foreach { case (keyE, value) =>
        val key = keyE.arr
        out.writeInt(key.length)
        out.write(key)
        out.writeInt(value.length)
        out.write(value)
      }
      out.toByteArray
    }

    def put(keyFun: DataOutput => Unit)(valueFun: DataOutput => Unit)(implicit tx: TxnLike): Unit =
      withIO { io =>
        val out   = io.out
        val key   = mkKey(out, keyFun)
        val value = mkArr(out, valueFun)
        map.put(key, value)
      }

    private def mkArr(out: DataOutput with ByteArrayStream, fun: DataOutput => Unit): Array[Byte] = {
      out.reset()
      fun(out)
      val size  = out.size
      val arr   = new Array[Byte](size)
      System.arraycopy(out.buffer, 0, arr, 0, size)
      arr
    }

    private def mkKey(out: DataOutput with ByteArrayStream, keyFun: DataOutput => Unit): Key = {
      val keyArr = mkArr(out, keyFun)
      new Key(keyArr)
    }

    def get[A](keyFun: DataOutput => Unit)(valueFun: DataInput => A)(implicit tx: TxnLike): Option[A] =
      withIO { io =>
        val key = mkKey(io.out, keyFun)
        map.get(key) match {
          case Some(arr) =>
            val in = DataInput(arr, 0, arr.length)  // XXX TODO: could also recycle with queue
            Some(valueFun(in))

          case None => None
        }
      }

    def flatGet[A](keyFun: DataOutput => Unit)(valueFun: DataInput => Option[A])(implicit tx: TxnLike): Option[A] =
      withIO { io =>
        val key = mkKey(io.out, keyFun)
        map.get(key) match {
          case Some(arr) =>
            val in = DataInput(arr, 0, arr.length)  // XXX TODO: could also recycle with queue
            valueFun(in)

          case None => None
        }
      }

    def contains(keyFun: DataOutput => Unit)(implicit tx: TxnLike): Boolean =
      withIO { io =>
        val key = mkKey(io.out, keyFun) // XXX TODO --- can we reuse something and avoid allocation?
        map.contains(key)
      }

    def remove(keyFun: DataOutput => Unit)(implicit tx: TxnLike): Boolean =
      withIO { io =>
        val out = io.out
        val key = mkKey(out, keyFun) // XXX TODO --- can we reuse something and avoid allocation?
        map.remove(key).isDefined
      }

    def close(): Unit = ()

    def numEntries(implicit tx: TxnLike): Int = map.size

    private[this] val ioQueue = new ConcurrentLinkedQueue[IO]

    private def withIO[A](fun: IO => A)(implicit tx: TxnLike): A = {
      val ioOld = ioQueue.poll()
      val io    = if (ioOld != null) ioOld else new IO
      try {
        fun(io)
      } finally {
        ioQueue.offer(io)
      }
    }
  }

  private[InMemoryDB] final class IO {
    final val out = DataOutput()
  }
}
trait InMemoryDB extends DataStore {
  def toByteArray(implicit tx: TxnLike): Array[Byte]
}