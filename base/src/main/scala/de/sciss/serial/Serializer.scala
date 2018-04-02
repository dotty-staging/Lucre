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

import java.lang.{String => JString}

import de.sciss.lucre.stm.Txn

import scala.annotation.switch
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.collection.mutable

object Serializer {
  implicit final val Unit   : ImmutableSerializer[scala.Unit    ] = ImmutableSerializer.Unit
  implicit final val Boolean: ImmutableSerializer[scala.Boolean ] = ImmutableSerializer.Boolean
  implicit final val Char   : ImmutableSerializer[scala.Char    ] = ImmutableSerializer.Char
  implicit final val Int    : ImmutableSerializer[scala.Int     ] = ImmutableSerializer.Int
  implicit final val Float  : ImmutableSerializer[scala.Float   ] = ImmutableSerializer.Float
  implicit final val Long   : ImmutableSerializer[scala.Long    ] = ImmutableSerializer.Long
  implicit final val Double : ImmutableSerializer[scala.Double  ] = ImmutableSerializer.Double
  implicit final val String : ImmutableSerializer[JString       ] = ImmutableSerializer.String

  implicit def immutable[Tx, Acc, A](implicit peer: ImmutableSerializer[A]): Serializer[Tx, Acc, A] =
    peer // peer.asInstanceOf[Serializer[T, A]] // XXX TODO: would create class-cast-exception

  // ---- higher-kinded ----

  // Option is not specialized at the moment
  implicit def option[Tx, Acc, A](implicit peer: Serializer[Tx, Acc, A]): Serializer[Tx, Acc, Option[A]] =
    new OptionWrapper[Tx, Acc, A](peer)

  private final class OptionWrapper[Tx, Acc, A](peer: Serializer[Tx, Acc, A])
    extends Serializer[Tx, Acc, Option[A]] {

    def write(opt: Option[A], out: DataOutput): Unit =
      opt match {
        case Some(v)  => out.writeByte(1); peer.write(v, out)
        case _        => out.writeByte(0)
      }

    def read(in: DataInput, acc: Acc)(implicit tx: Tx): Option[A] = (in.readByte(): @switch) match {
      case 1 => Some(peer.read(in, acc))
      case 0 => None
    }
  }
//
//  // Either is not specialized at the moment
//  implicit def either[T <: Txn, A, B](implicit peer1: Serializer[T, A],
//                                      peer2: Serializer[T, B]): Serializer[T, Either[A, B]] =
//    new EitherWrapper[T, A, B](peer1, peer2)
//
//  private final class EitherWrapper[T <: Txn, A, B](peer1: Serializer[T, A], peer2: Serializer[T, B])
//    extends Serializer[T, Either[A, B]] {
//
//    def write(either: Either[A, B], out: DataOutput): Unit =
//      either match {
//        case Left (a) => out.writeByte(0); peer1.write(a, out)
//        case Right(b) => out.writeByte(1); peer2.write(b, out)
//      }
//
//    def read(in: DataInput, tx: T)(implicit access: tx.Acc): Either[A, B] = (in.readByte(): @switch) match {
//      case 0 => Left (peer1.read(in, tx))
//      case 1 => Right(peer2.read(in, tx))
//    }
//  }
//
//  implicit def tuple2[T <: Txn, A1, A2]
//  (implicit peer1: Serializer[T, A1], peer2: Serializer[T, A2]): Serializer[T, (A1, A2) ] =
//    new Tuple2Wrapper[T, A1, A2](peer1, peer2)
//
//  private final class Tuple2Wrapper[T <: Txn, A1, A2]
//  (peer1: Serializer[T, A1], peer2: Serializer[T, A2])
//    extends Serializer[T, (A1, A2)] {
//
//    def write(tup: (A1, A2), out: DataOutput): Unit = {
//      peer1.write(tup._1, out)
//      peer2.write(tup._2, out)
//    }
//
//    def read(in: DataInput, tx: T)(implicit access: tx.Acc): (A1, A2) = {
//      val a1 = peer1.read(in, tx)
//      val a2 = peer2.read(in, tx)
//      (a1, a2)
//    }
//  }
//
//  implicit def tuple3[T <: Txn, A1, A2, A3](implicit peer1: Serializer[T, A1],
//                                            peer2: Serializer[T, A2],
//                                            peer3: Serializer[T, A3]): Serializer[T, (A1, A2, A3)] =
//    new Tuple3Wrapper[T, A1, A2, A3](peer1, peer2, peer3)
//
//  private final class Tuple3Wrapper[T <: Txn, A1, A2, A3](peer1: Serializer[T, A1],
//                                                          peer2: Serializer[T, A2],
//                                                          peer3: Serializer[T, A3])
//    extends Serializer[T, (A1, A2, A3)] {
//
//    def write(tup: (A1, A2, A3), out: DataOutput): Unit = {
//      peer1.write(tup._1, out)
//      peer2.write(tup._2, out)
//      peer3.write(tup._3, out)
//    }
//
//    def read(in: DataInput, tx: T)(implicit access: tx.Acc): (A1, A2, A3) = {
//      val a1 = peer1.read(in, tx)
//      val a2 = peer2.read(in, tx)
//      val a3 = peer3.read(in, tx)
//      (a1, a2, a3)
//    }
//  }
//
//  implicit def list[T <: Txn, A](implicit peer: Serializer[T, A]): Serializer[T, List[A]] =
//    new ListSerializer[T, A](peer)
//
//  implicit def set[T <: Txn, A](implicit peer: Serializer[T, A]): Serializer[T, Set[A]] =
//    new SetSerializer[T, A](peer)
//
//  implicit def indexedSeq[T <: Txn, A](implicit peer: Serializer[T, A]): Serializer[T, Vec[A]] =
//    new IndexedSeqSerializer[T, A](peer)
//
//  implicit def map[T <: Txn, A, B](implicit peer: Serializer[T, (A, B)]): Serializer[T, Map[A, B]] =
//    new MapSerializer[T, A, B](peer)
//
//  // XXX size might be a slow operation on That...
//  private abstract class CollectionSerializer[T <: Txn, A, That <: Traversable[A]] extends Serializer[T, That] {
//    def newBuilder: mutable.Builder[A, That]
//
//    def peer: Serializer[T, A]
//
//    final def write(coll: That, out: DataOutput): Unit = {
//      out.writeInt(coll.size)
//      val ser = peer
//      coll.foreach(ser.write(_, out))
//    }
//
//    final def read(in: DataInput, tx: T)(implicit access: tx.Acc): That = {
//      var sz = in.readInt()
//      val b = newBuilder
//      val ser = peer
//      while (sz > 0) {
//        b += ser.read(in, tx)
//        sz -= 1
//      }
//      b.result()
//    }
//  }
//
//  private final class ListSerializer[T <: Txn, A](val peer: Serializer[T, A])
//    extends CollectionSerializer[T, A, List[A]] {
//    def newBuilder: mutable.Builder[A, List[A]] = List.newBuilder[A]
//  }
//
//  private final class SetSerializer[T <: Txn, A](val peer: Serializer[T, A])
//    extends CollectionSerializer[T, A, Set[A]] {
//    def newBuilder: mutable.Builder[A, Set[A]] = Set.newBuilder[A]
//  }
//
//  private final class IndexedSeqSerializer[T <: Txn, A](val peer: Serializer[T, A])
//    extends CollectionSerializer[T, A, Vec[A]] {
//    def newBuilder: mutable.Builder[A, Vec[A]] = Vec.newBuilder[A]
//  }
//
//  private final class MapSerializer[T <: Txn, A, B](val peer: Serializer[T, (A, B)])
//    extends CollectionSerializer[T, (A, B), Map[A, B]] {
//    def newBuilder: mutable.Builder[(A, B), Map[A, B]] = Map.newBuilder[A, B]
//  }
}

trait Serializer[-Tx, -Acc, A] extends Reader[Tx, Acc, A] with Writer[A]
