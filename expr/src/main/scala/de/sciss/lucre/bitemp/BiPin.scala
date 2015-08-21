/*
 *  BiPin.scala
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

package de.sciss.lucre.bitemp

import de.sciss.lucre.event.Publisher
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.stm.{Disposable, Obj, Sys}
import de.sciss.model
import de.sciss.serial.{Writable, DataInput, Serializer}
import impl.{BiPinImpl => Impl}

import scala.collection.immutable.{IndexedSeq => Vec}

object BiPin extends Obj.Type {
  final val typeID = 25

  override def init(): Unit = {
    super.init()
    Entry.init()
  }

  final case class Update[S <: Sys[S], A](pin: BiPin[S, A], changes: List[Change[S, A]])

  object Entry extends Obj.Type {
    final val typeID = 26

    def apply[S <: Sys[S], A <: Obj[S]](key: Expr[S, Long], value: A)(implicit tx: S#Tx): Entry[S, A] =
      Impl.newEntry(key, value)

    def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] =
      Impl.readIdentifiedEntry(in, access)
  }
  trait Entry[S <: Sys[S], A] extends Publisher[S, model.Change[Long]] with Writable with Disposable[S#Tx] {
    def key  : Expr[S, Long]
    def value: A
  }

  // type Entry[S <: Sys[S], A] = (Expr[S, Long], A)
  type Leaf[S <: Sys[S], A] = Vec[Entry[S, A]]

  sealed trait Change[S <: Sys[S], A] {
    def entry: Entry[S, A]
  }

  final case class Added  [S <: Sys[S], A](time: Long              , entry: Entry[S, A]) extends Change[S, A]
  final case class Removed[S <: Sys[S], A](time: Long              , entry: Entry[S, A]) extends Change[S, A]
  final case class Moved  [S <: Sys[S], A](time: model.Change[Long], entry: Entry[S, A]) extends Change[S, A]

  object Modifiable {
    /** Extractor to check if a `BiPin` is actually a `BiPin.Modifiable`. */
    def unapply[S <: Sys[S], A](v: BiPin[S, A]): Option[Modifiable[S, A]] = {
      if (v.isInstanceOf[Modifiable[_, _]]) Some(v.asInstanceOf[Modifiable[S, A]]) else None
    }

    def read[S <: Sys[S], A <: Obj[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Modifiable[S, A] =
      Impl.readModifiable[S, A](in, access)

    def apply[S <: Sys[S], A <: Obj[S]](implicit tx: S#Tx): Modifiable[S, A] =
      Impl.newModifiable[S, A]

    implicit def serializer[S <: Sys[S], A <: Obj[S]]: Serializer[S#Tx, S#Acc, BiPin.Modifiable[S, A]] =
      Impl.modifiableSerializer[S, A]
  }

  trait Modifiable[S <: Sys[S], A] extends BiPin[S, A] {
    def add   (entry: Entry[S, A])(implicit tx: S#Tx): Unit
    def remove(entry: Entry[S, A])(implicit tx: S#Tx): Boolean
    def clear()(implicit tx: S#Tx): Unit
  }

  def read[S <: Sys[S], A <: Obj[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): BiPin[S, A] =
    Impl.read[S, A](in, access)

  implicit def serializer[S <: Sys[S], A <: Obj[S]]: Serializer[S#Tx, S#Acc, BiPin[S, A]] =
    Impl.serializer[S, A]

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] =
    Impl.readIdentifiedObj(in, access)
}

sealed trait BiPin[S <: Sys[S], A] extends Obj[S] with Publisher[S, BiPin.Update[S, A]] {
  import BiPin.{Entry, Leaf}

  def modifiableOption: Option[BiPin.Modifiable[S, A]]

  /** Queries the element valid for the given point in time.
    * Unlike, `intersect`, if there are multiple elements sharing
    * the same point in time, this returns the most recently added element.
    *
    * We propose that this should be the unambiguous way to evaluate
    * the `BiPin` for a given moment in time.
    *
    * @param time the query time point
    * @return  an element for the given time point, if it exists, otherwise `None`
    */
  def at(time: Long)(implicit tx: S#Tx): Option[Entry[S, A]]

  def valueAt(time: Long)(implicit tx: S#Tx): Option[A]

  /** Finds the entry at the given time, or the closest entry before the given time.
    *
    * @param time the query time
    * @return     the entry nearest in time to the query time, but not later than the
    *             query time, or `None` if there is no entry at such time
    */
  def floor(time: Long)(implicit tx: S#Tx): Option[Entry[S, A]]

  /** Finds the entry at the given time, or the closest entry after the given time.
    *
    * @param time the query time
    * @return     the entry nearest in time to the query time, but not earlier than the
    *             query time, or `None` if there is no entry at such time
    */
  def ceil(time: Long)(implicit tx: S#Tx): Option[Entry[S, A]]

  /** Queries all elements which are found at a given point in time.
    * There may be multiple time expressions which are not equal but
    * evaluate to the same moment in time. It is thus possible that
    * for a given point, multiple elements are found.
    *
    * @param time the query point
    * @return  the sequence of elements found along with their time expressions
    */
  def intersect(time: Long)(implicit tx: S#Tx): Leaf[S, A]

  /** Finds the entry with the smallest time which is greater than the query time.
    *
    * @param time the query time
    * @return     the time corresponding to the next entry, or `None` if there is no entry
    *             later than the given time
    */
  def eventAfter(time: Long)(implicit tx: S#Tx): Option[Long]

  def debugList()(implicit tx: S#Tx): List[(Long, A)]
}