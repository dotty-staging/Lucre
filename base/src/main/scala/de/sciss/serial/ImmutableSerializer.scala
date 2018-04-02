/*
 *  Serializer.scala
 *  (Serial)
 *
 * Copyright (c) 2011-2018 Hanns Holger Rutz. All rights reserved.
 *
 * This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 * For further information, please contact Hanns Holger Rutz at
 * contact@sciss.de
 */

package de.sciss
package serial

import scala.annotation.switch
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.collection.mutable

object ImmutableSerializer {

  // ---- primitives ----

  implicit object Unit extends ImmutableSerializer[scala.Unit] {
    def write(v: scala.Unit, out: DataOutput): Unit = ()
    def read(in: DataInput): Unit = ()
  }

  implicit object Boolean extends ImmutableSerializer[scala.Boolean] {
    def write(v: scala.Boolean, out: DataOutput): Unit = out.writeBoolean(v)

    def read(in: DataInput): scala.Boolean = in.readBoolean()
  }

  implicit object Char extends ImmutableSerializer[scala.Char] {
    def write(v: scala.Char, out: DataOutput): Unit = out.writeChar(v)

    def read(in: DataInput): scala.Char = in.readChar()
  }

  implicit object Int extends ImmutableSerializer[scala.Int] {
    def write(v: scala.Int, out: DataOutput): Unit = out.writeInt(v)

    def read(in: DataInput): scala.Int = in.readInt()
  }

  implicit object Float extends ImmutableSerializer[scala.Float] {
    def write(v: scala.Float, out: DataOutput): Unit = out.writeFloat(v)

    def read(in: DataInput): scala.Float = in.readFloat()
  }

  implicit object Long extends ImmutableSerializer[scala.Long] {
    def write(v: scala.Long, out: DataOutput): Unit = out.writeLong(v)

    def read(in: DataInput): scala.Long = in.readLong()
  }

  implicit object Double extends ImmutableSerializer[scala.Double] {
    def write(v: scala.Double, out: DataOutput): Unit = out.writeDouble(v)

    def read(in: DataInput): scala.Double = in.readDouble()
  }

  implicit object String extends ImmutableSerializer[java.lang.String] {
    def write(v: java.lang.String, out: DataOutput): Unit = out.writeUTF(v)

    def read(in: DataInput): java.lang.String = in.readUTF()
  }

  implicit object File extends ImmutableSerializer[java.io.File] {
    def write(v: java.io.File, out: DataOutput): Unit = out.writeUTF(v.getPath)

    def read(in: DataInput): java.io.File = new java.io.File(in.readUTF())
  }

  // ---- incremental build-up ----

  implicit def fromReader[A <: Writable](implicit reader: ImmutableReader[A]): ImmutableSerializer[A] =
    new ReaderWrapper(reader)

  private final class ReaderWrapper[A <: Writable](reader: ImmutableReader[A]) extends ImmutableSerializer[A] {
    def write(v: A, out: DataOutput): Unit = v.write(out)

    def read(in: DataInput): A = reader.read(in)
  }

  // ---- higher-kinded ----

  // Option is not specialized at the moment
  implicit def option[A](implicit peer: ImmutableSerializer[A]): ImmutableSerializer[Option[A]] =
    new OptionWrapper[A](peer)

  private final class OptionWrapper[A](peer: ImmutableSerializer[A])
    extends ImmutableSerializer[Option[A]] {

    def write(opt: Option[A], out: DataOutput): Unit =
      opt match {
        case Some(v)  => out.writeByte(1); peer.write(v, out)
        case _        => out.writeByte(0)
      }

    def read(in: DataInput): Option[A] = (in.readByte(): @switch) match {
      case 1 => Some(peer.read(in))
      case 0 => None
    }
  }

  implicit def either[A, B](implicit peer1: ImmutableSerializer[A],
                            peer2: ImmutableSerializer[B]): ImmutableSerializer[Either[A, B]] =
    new EitherWrapper[A, B](peer1, peer2)


  // Either is not specialized at the moment
  private final class EitherWrapper[A, B](peer1: ImmutableSerializer[A], peer2: ImmutableSerializer[B])
    extends ImmutableSerializer[Either[A, B]] {

    def write(either: Either[A, B], out: DataOutput): Unit =
      either match {
        case Left (a) => out.writeByte(0); peer1.write(a, out)
        case Right(b) => out.writeByte(1); peer2.write(b, out)
      }

    def read(in: DataInput): Either[A, B] = (in.readByte(): @switch) match {
      case 0 => Left (peer1.read(in))
      case 1 => Right(peer2.read(in))
    }
  }

  implicit def tuple2[A1, A2](implicit peer1: ImmutableSerializer[A1],
                              peer2: ImmutableSerializer[A2]): ImmutableSerializer[(A1, A2)] =
    new Tuple2Wrapper[A1, A2](peer1, peer2)

  private final class Tuple2Wrapper[A1, A2](peer1: ImmutableSerializer[A1],
                                            peer2: ImmutableSerializer[A2])
    extends ImmutableSerializer[(A1, A2)] {

    def write(tup: (A1, A2), out: DataOutput): Unit = {
      peer1.write(tup._1, out)
      peer2.write(tup._2, out)
    }

    def read(in: DataInput): (A1, A2) = {
      val a1 = peer1.read(in)
      val a2 = peer2.read(in)
      (a1, a2)
    }
  }

  // Tuple3 is not specialized at the moment
  implicit def tuple3[A1, A2, A3](implicit peer1: ImmutableSerializer[A1], peer2: ImmutableSerializer[A2],
                                  peer3: ImmutableSerializer[A3]): ImmutableSerializer[(A1, A2, A3)] =
    new Tuple3Wrapper[A1, A2, A3](peer1, peer2, peer3)

  private final class Tuple3Wrapper[A1, A2, A3](peer1: ImmutableSerializer[A1],
                                                peer2: ImmutableSerializer[A2],
                                                peer3: ImmutableSerializer[A3])
    extends ImmutableSerializer[(A1, A2, A3)] {

    def write(tup: (A1, A2, A3), out: DataOutput): Unit = {
      peer1.write(tup._1, out)
      peer2.write(tup._2, out)
      peer3.write(tup._3, out)
    }

    def read(in: DataInput): (A1, A2, A3) = {
      val a1 = peer1.read(in)
      val a2 = peer2.read(in)
      val a3 = peer3.read(in)
      (a1, a2, a3)
    }
  }

  implicit def list[A](implicit peer: ImmutableSerializer[A]): ImmutableSerializer[List[A]] =
    new ListSerializer[A](peer)

  implicit def set[A](implicit peer: ImmutableSerializer[A]): ImmutableSerializer[Set[A]] = new SetSerializer[A](peer)

  implicit def indexedSeq[A](implicit peer: ImmutableSerializer[A]): ImmutableSerializer[Vec[A]] =
    new IndexedSeqSerializer[A](peer)

  implicit def map[A, B](implicit peer: ImmutableSerializer[(A, B)]): ImmutableSerializer[Map[A, B]] =
    new MapSerializer[A, B](peer)

  // XXX size might be a slow operation on That...
  private sealed trait CollectionSerializer[A, That <: Traversable[A]] extends ImmutableSerializer[That] {
    def newBuilder: mutable.Builder[A, That]

    def peer: ImmutableSerializer[A]

    final def write(coll: That, out: DataOutput): Unit = {
      out.writeInt(coll.size)
      val ser = peer
      coll.foreach(ser.write(_, out))
    }

    final def read(in: DataInput): That = {
      var sz = in.readInt()
      val b = newBuilder
      val ser = peer
      while (sz > 0) {
        b += ser.read(in)
        sz -= 1
      }
      b.result()
    }
  }

  private final class ListSerializer[A](val peer: ImmutableSerializer[A])
    extends CollectionSerializer[A, List[A]] {
    def newBuilder: mutable.Builder[A, List[A]] = List.newBuilder[A]
  }

  private final class SetSerializer[A](val peer: ImmutableSerializer[A])
    extends CollectionSerializer[A, Set[A]] {
    def newBuilder: mutable.Builder[A, Set[A]] = Set.newBuilder[A]
  }

  private final class IndexedSeqSerializer[A](val peer: ImmutableSerializer[A])
    extends CollectionSerializer[A, Vec[A]] {
    def newBuilder: mutable.Builder[A, Vec[A]] = Vec.newBuilder[A]
  }

  private final class MapSerializer[A, B](val peer: ImmutableSerializer[(A, B)])
    extends CollectionSerializer[(A, B), Map[A, B]] {
    def newBuilder: mutable.Builder[(A, B), Map[A, B]] = Map.newBuilder[A, B]
  }
}

//trait NoTx extends Txn {
//  final type Self = NoTx
//}

trait ImmutableSerializer[A] extends ImmutableReader[A] with Serializer[Any, Any, A]