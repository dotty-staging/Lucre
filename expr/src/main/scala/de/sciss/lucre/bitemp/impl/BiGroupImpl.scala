/*
 *  BiGroupImpl.scala
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
package bitemp.impl

import de.sciss.lucre.bitemp.BiGroup
import de.sciss.lucre.data.{DeterministicSkipOctree, Iterator, SkipOctree}
import de.sciss.lucre.event.{impl => evti}
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.geom.LongSpace.TwoDim
import de.sciss.lucre.geom.{DistanceMeasure, LongDistanceMeasure2D, LongPoint2D, LongPoint2DLike, LongRectangle, LongSpace}
import de.sciss.lucre.stm.{Identifiable, NoSys, Obj, Sys}
import de.sciss.lucre.{event => evt}
import de.sciss.model.Change
import de.sciss.serial.{DataInput, DataOutput, Serializer}
import de.sciss.span.{Span, SpanLike}

import scala.annotation.elidable
import scala.collection.breakOut
import scala.collection.immutable.{IndexedSeq => Vec}

object BiGroupImpl {
  import BiGroup.{Leaf, MaxCoordinate, MaxSide, MaxSquare, MinCoordinate, Modifiable, TimedElem}

  def spanToPoint(span: SpanLike): LongPoint2D = span match {
    case Span(start, stop)  => LongPoint2D(start,     stop     )
    case Span.From(start)   => LongPoint2D(start,     MaxCoordinate)
    case Span.Until(stop)   => LongPoint2D(MinCoordinate, stop     )
    case Span.All           => LongPoint2D(MinCoordinate, MaxCoordinate)
    case Span.Void          => LongPoint2D(MaxCoordinate, MinCoordinate) // ?? what to do with this case ?? forbid?
  }

  def searchSpanToPoint(span: SpanLike): LongPoint2D = span match {
    case Span(start, stop)  => LongPoint2D(start,     stop         )
    case Span.From(start)   => LongPoint2D(start,     MaxCoordinate + 1)
    case Span.Until(stop)   => LongPoint2D(MinCoordinate, stop         )
    case Span.All           => LongPoint2D(MinCoordinate, MaxCoordinate + 1)
    case Span.Void          => LongPoint2D(MaxCoordinate, MinCoordinate    ) // ?? what to do with this case ?? forbid?
  }

  final def intersectTime[S <: Sys[S], A](tree: Tree[S, A])(time: Long)
                                             (implicit tx: S#Tx): Iterator[S#Tx, A] = {
    val start = time
    val stop = time + 1
    //         val shape = Rectangle( ti, MinCoordinate, MaxCoordinate - ti + 1, ti - MinCoordinate + 1 )
    // horizontally: until query_stop; vertically: from query_start
    // start < query.stop && stop > query.start
    val shape = LongRectangle(MinCoordinate, start + 1, stop - MinCoordinate, MaxCoordinate - start)
    tree.rangeQuery(shape)
  }

  final def intersectSpan[S <: Sys[S], A](tree: Tree[S, A])(span: SpanLike)
                                             (implicit tx: S#Tx): Iterator[S#Tx, A] = {
    // horizontally: until query_stop; vertically: from query_start
    span match {
      case Span(start, stop) =>
        val shape = LongRectangle(MinCoordinate, start + 1, stop - MinCoordinate, MaxCoordinate - start)
        tree.rangeQuery(shape)

      case Span.From(start) =>
        val shape = LongRectangle(MinCoordinate, start + 1, MaxSide, MaxCoordinate - start)
        tree.rangeQuery(shape)

      case Span.Until(stop) =>
        val shape = LongRectangle(MinCoordinate, MinCoordinate, stop - MinCoordinate, MaxSide)
        tree.rangeQuery(shape)

      case Span.All  => tree.iterator
      case Span.Void => Iterator.empty
    }
  }

  final def rangeSearch[S <: Sys[S], A](tree: Tree[S, A])(start: SpanLike, stop: SpanLike)
                                           (implicit tx: S#Tx): Iterator[S#Tx, A] = {
    if (start == Span.Void || stop == Span.Void) return Iterator.empty

    val startP = searchSpanToPoint(start)
    val stopP  = searchSpanToPoint(stop)
    val shape  = LongRectangle(startP.x, stopP.x, startP.y - startP.x /* + 1 */ , stopP.y - stopP.x /* + 1 */)
    //println( "RANGE " + shape )
    tree.rangeQuery(shape)
  }

  // this can be easily implemented with two rectangular range searches
  final def eventsAt[S <: Sys[S], A](tree: Tree[S, A])(time: Long)
                                        (implicit tx: S#Tx): (Iterator[S#Tx, A], Iterator[S#Tx, A]) = {
    val startShape = LongRectangle(time, MinCoordinate, 1, MaxSide)
    val stopShape  = LongRectangle(MinCoordinate, time, MaxSide, 1)
    (tree.rangeQuery(startShape), tree.rangeQuery(stopShape))
  }

  final def eventAfter[S <: Sys[S], T2](tree: Tree[S, (SpanLike, T2)])(time: Long)
                                           (implicit tx: S#Tx): Option[Long] = {
    val t1    = time + 1
    val point = LongPoint2D(t1, t1) // + 1
    val span  = tree.nearestNeighborOption(point, AdvanceNextNeighborMetric).map(_._1).getOrElse(Span.Void)
    span match {
      case sp @ Span.From(start) => assert(start >= t1, sp); Some(start) // else None
      case sp @ Span.Until(stop) => assert(stop  >= t1, sp); Some(stop ) // else None
      case sp @ Span(start, stop) =>
        if (start >= t1) {
          Some(start)
        } else {
          assert(stop >= t1, sp)
          Some(stop)
        }
      case _ => None // All or Void
    }
  }

  final def eventBefore[S <: Sys[S], T2](tree: Tree[S, (SpanLike, T2)])(time: Long)
                                        (implicit tx: S#Tx): Option[Long] = {
    val t1    = time - 1
    val point = LongPoint2D(t1, t1)
    val span  = tree.nearestNeighborOption(point, RegressNextNeighborMetric).map(_._1).getOrElse(Span.Void)
    span match {
      case sp @ Span.From(start)  => assert(start <= t1, sp); Some(start) // else None
      case sp @ Span.Until(stop)  => assert(stop  <= t1, sp); Some(stop ) // else None
      case sp @ Span(start, stop) =>
        if (stop <= t1) {
          Some(stop)
        } else {
          assert(start <= t1, sp)
          Some(start)
        }
      case _ => None // All or Void
    }
  }

  // ... accepted are points with x > LRP || y > LRP ...
  final val AdvanceNextNeighborMetric: DistanceMeasure.Ops[Long, LongSpace.TwoDim] = LongDistanceMeasure2D.nextSpanEvent(MaxSquare)
  final val RegressNextNeighborMetric: DistanceMeasure.Ops[Long, LongSpace.TwoDim] = LongDistanceMeasure2D.prevSpanEvent(MaxSquare)

  var showLog = false

  @elidable(elidable.CONFIG) private def log(what: => String): Unit =
    if (showLog) println(s"<bigroup> $what")

  type Tree    [S <: Sys[S], A          ] = SkipOctree[S, TwoDim, A]
  type LeafImpl[S <: Sys[S], A <: Obj[S]] = (SpanLike, Vec[TimedElemImpl[S, A]])
  type TreeImpl[S <: Sys[S], A <: Obj[S]] = SkipOctree[S, TwoDim, LeafImpl[S, A]]

  def verifyConsistency[S <: Sys[S], A](group: BiGroup[S, A], reportOnly: Boolean)
                                       (implicit tx: S#Tx): Vec[String] =
    group match {
      case impl: Impl[S, A] => impl.treeHandle match {
        case t: DeterministicSkipOctree[S, _, _] =>
          DeterministicSkipOctree.verifyConsistency(t, reportOnly)
        case _ => sys.error("Not a deterministic octree implementation")
      }
    }

  def serializer[S <: Sys[S], A <: Obj[S]]: Serializer[S#Tx, S#Acc, BiGroup[S, A]] = anySer.asInstanceOf[Ser[S, A]]

  private val anySer = new Ser[NoSys, Obj[NoSys]]

  def modifiableSerializer[S <: Sys[S], A <: Obj[S]]: Serializer[S#Tx, S#Acc, BiGroup.Modifiable[S, A]] =
    anyModSer.asInstanceOf[ModSer[S, A]]

  private val anyModSer = new ModSer[NoSys, Obj[NoSys]]

  def readModifiable[S <: Sys[S], A <: Obj[S]](in: DataInput, access: S#Acc)
                                              (implicit tx: S#Tx): BiGroup.Modifiable[S, A] = {
    val targets = evt.Targets.read[S](in, access)
    read(in, access, targets)
  }

  private class Ser[S <: Sys[S], A <: Obj[S]] extends Obj.Serializer[S, BiGroup[S, A]] {
    protected def typeID: Int = BiGroup.typeID

    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): BiGroup[S, A] = {
      BiGroupImpl.read(in, access, targets)
    }
  }

  private class ModSer[S <: Sys[S], A <: Obj[S]] extends Obj.Serializer[S, BiGroup.Modifiable[S, A]] {
    protected def typeID: Int = BiGroup.typeID

    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): BiGroup.Modifiable[S, A] = {
      BiGroupImpl.read(in, access, targets)
    }
  }

  private[lucre] final class TimedElemImpl[S <: Sys[S], A <: Obj[S]](group          : Impl[S, A],
                                                             protected val targets : evt.Targets[S],
                                                             val span              : Expr[S, SpanLike],
                                                             val value             : A)
    extends evti.SingleNode[S, Change[SpanLike]] with TimedElem[S, A] {

    def typeID: Int = TimedElem.typeID

    override def toString = s"TimedElem$id"

    /** Tricky override to allow comparison with BiGroup.TimedElem.Wrapper */
    override def equals(that: Any): Boolean = that match {
      case m: Identifiable[_] => this.id == m.id
      case _ => super.equals(that)
    }

    object changed extends Changed with evti.Root[S, Change[SpanLike]]

    protected def writeData(out: DataOutput): Unit = {
      span .write(out)
      value.write(out)
    }

    protected def disposeData()(implicit tx: S#Tx) = disconnect()

    def connect()(implicit tx: S#Tx): this.type = {
      log(s"$this.connect()")
      span.changed ---> changed
      this
    }

    private[this] def disconnect()(implicit tx: S#Tx): Unit = {
      log(s"$this.disconnect()")
      span.changed -/-> changed
    }
  }

  abstract class Impl[S <: Sys[S], A <: Obj[S]]
    extends Modifiable[S, A]
    with evt.impl.SingleNode[S, BiGroup.Update[S, A]] {

    group =>

    // ---- abstract ----

    implicit final def pointView: (Leaf[S, A], S#Tx) => LongPoint2DLike = (tup, tx) => spanToPoint(tup._1)

    protected def tree: TreeImpl[S, A]

    // ---- implemented ----

    def typeID: Int = BiGroup.typeID

    final def treeHandle = tree

    override def toString: String = s"BiGroup${tree.id}"

    def modifiableOption: Option[BiGroup.Modifiable[S, A]] = Some(this)

    implicit object TimedSer extends Obj.Serializer[S, TimedElemImpl[S, A]] {
      def typeID: Int = TimedElem.typeID

      def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): TimedElemImpl[S, A] = {
        val span  = expr.SpanLike .read(in, access)
        val value = Obj           .read(in, access).asInstanceOf[A]
        new TimedElemImpl(group, targets, span, value)
      }
    }

    // Note: must be after `TimedSer`
    protected final def newTree()(implicit tx: S#Tx): TreeImpl[S, A] =
      SkipOctree.empty[S, TwoDim, LeafImpl[S, A]](BiGroup.MaxSquare)

    // Note: must be after `TimedSer`
    protected final def readTree(in: DataInput, access: S#Acc)(implicit tx: S#Tx): TreeImpl[S, A] =
      SkipOctree.read[S, TwoDim, LeafImpl[S, A]](in, access)

    // ---- event behaviour ----

    object changed extends Changed with evt.impl.Generator[S, BiGroup.Update[S, A]] {
      def += (elem: TimedElemImpl[S, A])(implicit tx: S#Tx): Unit = elem.changed ---> this
      def -= (elem: TimedElemImpl[S, A])(implicit tx: S#Tx): Unit = elem.changed -/-> this

      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[BiGroup.Update[S, A]] = {
        val par = pull.parents(this)
        log(s"$this.pullUpdate -> parents = $par")

        val changes: List[BiGroup.Moved[S, A]] = par.flatMap { evt =>
          val timed = evt.node.asInstanceOf[TimedElemImpl[S, A]]
          val ch0   = pull(timed.changed)
          log(s"$this.pullUpdate -> from timed $timed pulled $ch0")
          ch0.map {
            case ch @ Change(spanValOld, spanValNew) =>
              assert(removeNoFire(spanValOld, timed))
              addNoFire          (spanValNew, timed)
              BiGroup.Moved(ch, timed)
          }
        } (breakOut)

        if (changes.isEmpty) None else Some(BiGroup.Update(group, changes))
      }
    }

    final protected def disposeData()(implicit tx: S#Tx): Unit = {
      tree.iterator.foreach { case (_, xs) =>
        xs.foreach(_.dispose())
      }
      tree.dispose()
    }

    final protected def writeData(out: DataOutput): Unit = tree.write(out)

    // ---- collection behaviour ----

    final def clear()(implicit tx: S#Tx): Unit = {
      val changes = tree.iterator.toList.flatMap {
        case (spanVal, seq) =>
          seq.map {
            timed => BiGroup.Removed(spanVal, timed)
          }
      }
      tree.clear()
      if (changes.nonEmpty) changed.fire(BiGroup.Update(group, changes))
    }

    final def add(span: Expr[S, SpanLike], elem: A)(implicit tx: S#Tx): TimedElem[S, A] = {
      log(s"$this.add($span, $elem)")
      val spanVal = span.value
      val tgt     = evt.Targets[S]
      val timed   = new TimedElemImpl[S, A](group, tgt, span, elem).connect()
      addNoFire(spanVal, timed)

      changed += timed
      changed.fire(BiGroup.Update(group, BiGroup.Added(spanVal, timed) :: Nil))

      timed
    }

    private def addNoFire(spanVal: SpanLike, timed: TimedElemImpl[S, A])(implicit tx: S#Tx): Unit = {
      val point = spanToPoint(spanVal)
      //if( showLog ) println( "add at point " + point )
      //         val entry   = (span, elem)
      tree.transformAt(point) {
        case None           => Some(spanVal -> Vec    (timed))
        case Some((_, seq)) => Some(spanVal -> (seq :+ timed))
      }
    }

    final def remove(span: Expr[S, SpanLike], elem: A)(implicit tx: S#Tx): Boolean = {
      val spanVal = span.value
      val point   = spanToPoint(spanVal)
      val timedO  = tree.get(point).flatMap {
        case (_, Vec(single)) =>
          if (single.span == span && single.value == elem) {
            tree.removeAt(point)
            Some(single)
          } else {
            None
          }
        case (_, seq) =>
          val (equal, diff) = seq.partition(timed => timed.span == span && timed.value == elem)
          if (equal.nonEmpty) {
            tree.add((spanVal, diff))
            equal.headOption
          } else {
            None
          }
      }

      timedO.foreach { timed =>
        changed -= timed
        changed.fire(BiGroup.Update(group, BiGroup.Removed(spanVal, timed) :: Nil))
        timed.dispose()
      }

      timedO.isDefined
    }

    private def removeNoFire(spanVal: SpanLike, timed: TimedElemImpl[S, A])(implicit tx: S#Tx): Boolean = {
      val point = spanToPoint(spanVal)
      val entry = tree.get(point)
      entry match {
        case Some((_, Vec(single))) =>
          if (single == timed) {
            assert(tree.removeAt(point).isDefined)
            true
          } else {
            false
          }
        case Some((_, seq)) =>
          val seqNew = seq.filterNot(_ == timed)
          if (seqNew.size != seq.size) {
            assert(tree.add((spanVal, seqNew)))
            true
          } else {
            false
          }
        case None => false
      }
    }

    final def debugList(implicit tx: S#Tx): List[(SpanLike, A)] =
      tree.toList.flatMap {
        case (span, seq) => seq.map(span -> _.value)
      }

    final def debugPrint(implicit tx: S#Tx): String = tree.debugPrint()

    final def iterator(implicit tx: S#Tx): data.Iterator[S#Tx, Leaf[S, A]] = tree.iterator

    final def intersect(time: Long)(implicit tx: S#Tx): Iterator[S#Tx, Leaf[S, A]] =
      BiGroupImpl.intersectTime(tree)(time)

    final def intersect(span: SpanLike)(implicit tx: S#Tx): Iterator[S#Tx, Leaf[S, A]] =
      BiGroupImpl.intersectSpan(tree)(span)

    final def rangeSearch(start: SpanLike, stop: SpanLike)(implicit tx: S#Tx): Iterator[S#Tx, Leaf[S, A]] =
      BiGroupImpl.rangeSearch(tree)(start, stop)

    // this can be easily implemented with two rectangular range searches
    final def eventsAt(time: Long)(implicit tx: S#Tx): (Iterator[S#Tx, Leaf[S, A]], Iterator[S#Tx, Leaf[S, A]]) =
      BiGroupImpl.eventsAt(tree)(time)

    final def eventAfter(time: Long)(implicit tx: S#Tx): Option[Long] =
      BiGroupImpl.eventAfter(tree)(time)

    final def eventBefore(time: Long)(implicit tx: S#Tx): Option[Long] =
      BiGroupImpl.eventBefore(tree)(time)

    final def firstEvent(implicit tx: S#Tx): Option[Long] =
      BiGroupImpl.eventAfter(tree)(BiGroup.MinCoordinate)

    final def lastEvent(implicit tx: S#Tx): Option[Long] =
      BiGroupImpl.eventBefore(tree)(BiGroup.MaxCoordinate)
  }

  def newModifiable[S <: Sys[S], A <: Obj[S]](implicit tx: S#Tx): Modifiable[S, A] =
    new Impl[S, A] {
      protected val targets = evt.Targets[S]

      val tree: TreeImpl[S, A] = newTree()
    }

  private def read[S <: Sys[S], A <: Obj[S]](in: DataInput, access: S#Acc, _targets: evt.Targets[S])
                                            (implicit tx: S#Tx): Impl[S, A] =
    new Impl[S, A] {
      protected val targets: evt.Targets[S] = _targets

      val tree: TreeImpl[S, A] = readTree(in, access)
    }
}