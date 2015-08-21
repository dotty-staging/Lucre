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
import de.sciss.lucre.data.Iterator
import de.sciss.lucre.event.{EventLike, Publisher}
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.geom.LongSquare
import de.sciss.lucre.stm.{Obj, Identifiable, Sys}
import de.sciss.lucre.{event => evt}
import de.sciss.serial.{DataInput, Serializer}
import de.sciss.span.{SpanLike => SpanLikeV}
import de.sciss.{model => m}

import scala.collection.immutable.{IndexedSeq => Vec}

object BiGroup extends Obj.Type {
  final val MaxSquare     = LongSquare(0, 0, 0x2000000000000000L)
  final val MaxSide       = MaxSquare.side
  final val MinCoordinate = MaxSquare.left
  final val MaxCoordinate = MaxSquare.right

  final val typeID = 27

  override def init(): Unit = {
    super.init()
    TimedElem.init()
  }

  // ---- updates ----

  final case class Update[S <: Sys[S], A](group: BiGroup[S, A], changes: List[Change[S, A]])

  sealed trait Change[S <: Sys[S], +A] {
    def elem: TimedElem[S, A]
  }

  final case class Added[S <: Sys[S], A](span: SpanLikeV /* Span.HasStart */ , elem: TimedElem[S, A])
    extends Change[S, A]

  final case class Removed[S <: Sys[S], A](span: SpanLikeV /* Span.HasStart */ , elem: TimedElem[S, A])
    extends Change[S, A]

  final case class Moved[S <: Sys[S], A](change: m.Change[SpanLikeV], elem: TimedElem[S, A])
    extends Change[S, A]

  // ---- structural data ----

  type Leaf[S <: Sys[S], +A] = (SpanLikeV /* Span.HasStart */ , Vec[TimedElem[S, A]])

  object TimedElem extends Obj.Type {
    final val typeID = 28

    def apply[S <: Sys[S], A](id: S#ID, span: Expr[S, SpanLikeV], value: A): TimedElem[S, A] =
      Wrapper(id, span, value)

    private final case class Wrapper[S <: Sys[S], A](id: S#ID, span: Expr[S, SpanLikeV], value: A)
      extends TimedElem[S, A] {

      override def toString = s"TimedElem$id"

      override def equals(that: Any): Boolean = that match {
        case m: Identifiable[_] => this.id == m.id
        case _ => super.equals(that)
      }

      override def hashCode = id.hashCode()
    }

    def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] =
      Impl.readIdentifiedTimed(in, access)
  }

  trait TimedElem[S <: Sys[S], +A] extends Identifiable[S#ID] {
    def span : Expr[S, SpanLikeV]
    def value: A

    override def toString = s"TimedElem($id, $span, $value)"
  }

  object Modifiable {
    implicit def serializer[S <: Sys[S], A <: Obj[S]]: Serializer[S#Tx, S#Acc, BiGroup.Modifiable[S, A]] =
      Impl.modifiableSerializer[S, A]

    def apply[S <: Sys[S], A <: Obj[S]](implicit tx: S#Tx): Modifiable[S, A] =
      Impl.newModifiable[S, A]

    def read[S <: Sys[S], A <: Obj[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Modifiable[S, A] =
      Impl.readModifiable(in, access)
  }

  trait Modifiable[S <: Sys[S], A] extends BiGroup[S, A] {
    def add(span: Expr[S, SpanLikeV], elem: A)(implicit tx: S#Tx): TimedElem[S, A]

    def remove(span: Expr[S, SpanLikeV], elem: A)(implicit tx: S#Tx): Boolean

    def clear()(implicit tx: S#Tx): Unit

    override def changed: EventLike[S, BiGroup.Update[S, A]]
  }

  implicit def serializer[S <: Sys[S], A <: Obj[S]]: Serializer[S#Tx, S#Acc, BiGroup[S, A]] =
    Impl.serializer[S, A]

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] =
    Impl.readIdentifiedObj(in, access)
}

trait BiGroup[S <: Sys[S], A] extends evt.Node[S] with Publisher[S, BiGroup.Update[S, A]] {

  import BiGroup.Leaf

  def modifiableOption: Option[BiGroup.Modifiable[S, A]]

  def iterator(implicit tx: S#Tx): data.Iterator[S#Tx, Leaf[S, A]]

  /** Queries all elements intersecting a given point in time.
    * That is, returns an iterator of all elements whose span contains the time point
    * `(span start <= time && span.stop > time)`
    *
    * This methods makes no guarantees about the ordering of the returned iterator.
    *
    * @param time the point in time to search at
    * @return  a (possibly empty) iterator of the intersecting elements
    */
  def intersect(time: Long)(implicit tx: S#Tx): Iterator[S#Tx, Leaf[S, A]]

  /** Queries all elements intersecting a given time span.
    * That is, returns an iterator of all elements whose span contains or partly overlaps the query span.
    * `(span start < query.stop && span.stop > query.start)`
    *
    * This methods makes no guarantees about the ordering of the returned iterator.
    *
    * @param span the the span to search within (this may be a half-bounded interval or even `Span.All`)
    * @return  a (possibly empty) iterator of the intersecting elements
    */
  def intersect(span: SpanLikeV)(implicit tx: S#Tx): Iterator[S#Tx, Leaf[S, A]]

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
  def rangeSearch(start: SpanLikeV, stop: SpanLikeV)(implicit tx: S#Tx): Iterator[S#Tx, Leaf[S, A]]

  /** Queries the closest event (an element's span starting or stopping) at the given time or later
    *
    * @param time the query time
    * @return a time, greater than or equal to the query time, at which the next event occurs, or `None` if
    *         there are no events at or after the query time
    */
  def eventAfter(time: Long)(implicit tx: S#Tx): Option[Long]

  /** Queries the closest event (an element's span starting or stopping) at the given time or earlier
    *
    * @param time the query time
    * @return a time, smaller than or equal to the query time, at which the previous event occurs, or `None` if
    *         there are no events at or before the query time
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
  def eventsAt(time: Long)(implicit tx: S#Tx): (Iterator[S#Tx, Leaf[S, A]], Iterator[S#Tx, Leaf[S, A]])

  def debugList(implicit tx: S#Tx): List[(SpanLikeV, A)]

  def debugPrint(implicit tx: S#Tx): String
}