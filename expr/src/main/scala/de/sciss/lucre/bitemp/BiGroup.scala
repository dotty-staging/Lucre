/*
 *  BiGroup.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2015 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre
package bitemp

import de.sciss.lucre.bitemp.impl.{BiGroupImpl => Impl}
import de.sciss.lucre.event.{EventLike, Publisher}
import de.sciss.lucre.expr.SpanLikeObj
import de.sciss.lucre.geom.LongSquare
import de.sciss.lucre.stm.{Sys, Elem, Obj}
import de.sciss.serial.{DataInput, Serializer}
import de.sciss.span.SpanLike
import de.sciss.{model => m}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.language.higherKinds

object BiGroup extends Obj.Type {
  final val MaxSquare     = LongSquare(0, 0, 0x2000000000000000L)
  final val MaxSide       = MaxSquare.side
  final val MinCoordinate = MaxSquare.left
  final val MaxCoordinate = MaxSquare.right

  final val typeID = 27

  override def init(): Unit = {
    super.init()
    Entry.init()
  }

  // ---- updates ----

  final case class Update[S <: Sys[S], A](group: BiGroup[S, A], changes: List[Change[S, A]])

  sealed trait Change[S <: Sys[S], +A] {
    def elem: Entry[S, A]
  }

  final case class Added[S <: Sys[S], A](span: SpanLike /* Span.HasStart */ , elem: Entry[S, A])
    extends Change[S, A]

  final case class Removed[S <: Sys[S], A](span: SpanLike /* Span.HasStart */ , elem: Entry[S, A])
    extends Change[S, A]

  final case class Moved[S <: Sys[S], A](change: m.Change[SpanLike], elem: Entry[S, A])
    extends Change[S, A]

  // ---- structural data ----

  type Leaf[S <: Sys[S], +A] = (SpanLike /* Span.HasStart */ , Vec[Entry[S, A]])

  // XXX TODO --- eventually we might drop Obj in favour of Elem
  object Entry extends Obj.Type {
    final val typeID = 28

    def unapply[S <: Sys[S], A](entry: Entry[S, A]): Entry[S, A] = entry

    implicit def serializer[S <: Sys[S], A <: Elem[S]]: Serializer[S#Tx, S#Acc, Entry[S, A]] =
      Impl.entrySer[S, A]

    def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] =
      Impl.readIdentifiedEntry(in, access)
  }

  trait Entry[S <: Sys[S], +A] extends Obj[S] with Publisher[S, m.Change[SpanLike]] {
    def span : SpanLikeObj[S]
    def value: A

    def isEmpty: Boolean = false
    def get: (SpanLikeObj[S], A) = (span, value)
  }

  object Modifiable {
    implicit def serializer[S <: Sys[S], A <: Elem[S]]: Serializer[S#Tx, S#Acc, BiGroup.Modifiable[S, A]] =
      Impl.modifiableSerializer[S, A]

    def apply[S <: Sys[S], E[~ <: Sys[~]] <: Elem[~]](implicit tx: S#Tx): Modifiable[S, E[S]] =
      Impl.newModifiable[S, E]

    def read[S <: Sys[S], A <: Elem[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Modifiable[S, A] =
      serializer[S, A].read(in, access)
  }

  trait Modifiable[S <: Sys[S], A] extends BiGroup[S, A] {
    def add(span: SpanLikeObj[S], elem: A)(implicit tx: S#Tx): Entry[S, A]

    def remove(span: SpanLikeObj[S], elem: A)(implicit tx: S#Tx): Boolean

    def clear()(implicit tx: S#Tx): Unit

    override def changed: EventLike[S, BiGroup.Update[S, A]]
  }

  implicit def serializer[S <: Sys[S], A <: Elem[S]]: Serializer[S#Tx, S#Acc, BiGroup[S, A]] =
    Impl.serializer[S, A]

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] =
    Impl.readIdentifiedObj(in, access)
}

trait BiGroup[S <: Sys[S], A] extends Obj[S] with Publisher[S, BiGroup.Update[S, A]] {

  import BiGroup.Leaf

  def modifiableOption: Option[BiGroup.Modifiable[S, A]]

  def iterator(implicit tx: S#Tx): Iterator[Leaf[S, A]]

  /** Queries all elements intersecting a given point in time.
    * That is, returns an iterator of all elements whose span contains the time point
    * `(span start <= time && span.stop > time)`
    *
    * This methods makes no guarantees about the ordering of the returned iterator.
    *
    * @param time the point in time to search at
    * @return  a (possibly empty) iterator of the intersecting elements
    */
  def intersect(time: Long)(implicit tx: S#Tx): Iterator[Leaf[S, A]]

  /** Queries all elements intersecting a given time span.
    * That is, returns an iterator of all elements whose span contains or partly overlaps the query span.
    * `(span start < query.stop && span.stop > query.start)`
    *
    * This methods makes no guarantees about the ordering of the returned iterator.
    *
    * @param span the the span to search within (this may be a half-bounded interval or even `Span.All`)
    * @return  a (possibly empty) iterator of the intersecting elements
    */
  def intersect(span: SpanLike)(implicit tx: S#Tx): Iterator[Leaf[S, A]]

  /** Performs a range query according to separate intervals for the allowed start and stop positions
    * of the element spans. That is, returns an iterator of all elements whose span satisfies the
    * constraints given for start and stop positions
    * `(start.contains( elem.span.start ) && stop.contains( elem.span.stop ))`
    *
    * Both for the start and stop constraint, half-bounded or unbounded (`Span.All`) intervals can be used.
    * Examples
    *
    * - to find all elements which start between 10 (inclusive) and 20 (exclusive), use `start = Span( 10, 20 ), stop = Span.All`.
    * - to find all elements which start before (<) 10 and stop from (>=) 20, use `start = Span.until( 10 ), stop = Span.from( 20 )`.
    *
    * This methods makes no guarantees about the ordering of the returned iterator.
    *
    * @param start   the constraint for the start position of the spans of the elements filtered.
    * @param stop    the constraint for the stop position of the spans of the elements filtered.
    * @return  a (possibly empty) iterator of the intersecting elements
    */
  def rangeSearch(start: SpanLike, stop: SpanLike)(implicit tx: S#Tx): Iterator[Leaf[S, A]]

  /** Queries the closest event (an element's span starting or stopping) at the given time or later
    *
    * @param time the query time
    * @return a time, greater than the query time, at which the next event occurs, or `None` if
    *         there are no events after the query time
    */
  def eventAfter(time: Long)(implicit tx: S#Tx): Option[Long]

  /** Queries the closest event (an element's span starting or stopping) at the given time or earlier
    *
    * @param time the query time
    * @return a time, smaller than the query time, at which the previous event occurs, or `None` if
    *         there are no events before the query time
    */
  def eventBefore(time: Long)(implicit tx: S#Tx): Option[Long]

  /** Finds the first occurring event, if there is any. Ignores objects with `Span.All`. */
  def firstEvent(implicit tx: S#Tx): Option[Long]

  /** Finds the last occurring event, if there is any. Ignores objects with `Span.All`. */
  def lastEvent (implicit tx: S#Tx): Option[Long]

  /** Queries all elements which produce an event (starting or stopping) at a given time.
    *
    * @param time the time instant for which to gather the events
    * @return  a tuple of two iterators. the first iterator (`_1`) contains the events which
    *          start at the query time, the second iterator (`_2`) contains the event which
    *          stop at the query time
    */
  def eventsAt(time: Long)(implicit tx: S#Tx): (Iterator[Leaf[S, A]], Iterator[Leaf[S, A]])

  def debugList(implicit tx: S#Tx): List[(SpanLike, A)]

  def debugPrint(implicit tx: S#Tx): String
}