/*
 *  BiGroupImpl.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2020 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre
package bitemp.impl

import de.sciss.equal.Implicits._
import de.sciss.lucre.bitemp.BiGroup
import de.sciss.lucre.data.{DeterministicSkipOctree, SkipOctree}
import de.sciss.lucre.event.{Targets, impl => evti}
import de.sciss.lucre.expr.SpanLikeObj
import de.sciss.lucre.geom.LongSpace.TwoDim
import de.sciss.lucre.geom.{DistanceMeasure, LongDistanceMeasure2D, LongPoint2D, LongPoint2DLike, LongRectangle, LongSpace}
import de.sciss.lucre.stm.impl.ObjSerializer
import de.sciss.lucre.stm.{Copy, Elem, NoSys, Obj, Sys}
import de.sciss.lucre.{event => evt}
import de.sciss.model.Change
import de.sciss.serial.{DataInput, DataOutput, Serializer}
import de.sciss.span.{Span, SpanLike}

import scala.annotation.elidable
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.language.higherKinds

object BiGroupImpl {
  import BiGroup.{Entry, Leaf, MaxCoordinate, MaxSide, MaxSquare, MinCoordinate, Modifiable}

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
                                             (implicit tx: S#Tx): Iterator[A] = {
    val start = time
    val stop = time + 1
    //         val shape = Rectangle( ti, MinCoordinate, MaxCoordinate - ti + 1, ti - MinCoordinate + 1 )
    // horizontally: until query_stop; vertically: from query_start
    // start < query.stop && stop > query.start
    val shape = LongRectangle(MinCoordinate, start + 1, stop - MinCoordinate, MaxCoordinate - start)
    tree.rangeQuery(shape)
  }

  final def intersectSpan[S <: Sys[S], A](tree: Tree[S, A])(span: SpanLike)
                                             (implicit tx: S#Tx): Iterator[A] = {
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
                                           (implicit tx: S#Tx): Iterator[A] = {
    if (start === Span.Void || stop === Span.Void) return Iterator.empty

    val startP = searchSpanToPoint(start)
    val stopP  = searchSpanToPoint(stop)
    val shape  = LongRectangle(startP.x, stopP.x, startP.y - startP.x /* + 1 */ , stopP.y - stopP.x /* + 1 */)
    //println( "RANGE " + shape )
    tree.rangeQuery(shape)
  }

  // this can be easily implemented with two rectangular range searches
  final def eventsAt[S <: Sys[S], A](tree: Tree[S, A])(time: Long)
                                        (implicit tx: S#Tx): (Iterator[A], Iterator[A]) = {
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
  type LeafImpl[S <: Sys[S], E[~ <: Sys[~]] <: Elem[~]] = (SpanLike, Vec[Entry[S, E[S]]])
  type TreeImpl[S <: Sys[S], E[~ <: Sys[~]] <: Elem[~]] = SkipOctree[S, TwoDim, LeafImpl[S, E]]
  // type EntryRepr[Repr[~] <: Elem[~]]

  def verifyConsistency[S <: Sys[S], A](group: BiGroup[S, A], reportOnly: Boolean)
                                       (implicit tx: S#Tx): Vec[String] =
    group match {
      case impl: Impl[S, _, _] =>   // wrong warning
        impl.treeHandle match {
          case t: DeterministicSkipOctree[S, _, _] =>
            t.verifyConsistency(reportOnly)
          case _ => sys.error("Not a deterministic octree implementation")
        }
    }

  def serializer[S <: Sys[S], A <: Elem[S]]: Serializer[S#Tx, S#Acc, BiGroup[S, A]] = anySer.asInstanceOf[Ser[S, A]]

  private val anySer = new Ser[NoSys, Obj[NoSys]]

  def modifiableSerializer[S <: Sys[S], A <: Elem[S]]: Serializer[S#Tx, S#Acc, BiGroup.Modifiable[S, A]] =
    anyModSer.asInstanceOf[ModSer[S, A]]

  private val anyModSer = new ModSer[NoSys, Obj[NoSys]]

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] = {
    val targets = Targets.read(in, access)
    read(in, access, targets)
  }

  def readIdentifiedEntry[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] = {
    val targets = Targets.read(in, access)
    readEntry(in, access, targets)
  }

  private class Ser[S <: Sys[S], A <: Elem[S]] extends ObjSerializer[S, BiGroup[S, A]] {
    def tpe: Obj.Type = BiGroup
  }

  private class ModSer[S <: Sys[S], A <: Elem[S]] extends ObjSerializer[S, BiGroup.Modifiable[S, A]] {
    def tpe: Obj.Type = BiGroup
  }

  private[lucre] final class EntryImpl[S <: Sys[S], A <: Elem[S]](val targets : evt.Targets[S],
                                                                  val span : SpanLikeObj[S],
                                                                  val value: A)
    extends evti.SingleNode[S, Change[SpanLike]] with Entry[S, A] {

    def tpe: Obj.Type = Entry

    override def toString = s"Entry($id, $span, $value)"

    object changed extends Changed with evti.Root[S, Change[SpanLike]]

    private[lucre] def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
      new EntryImpl(Targets[Out], context(span), context[Elem](value)).connect()

    protected def writeData(out: DataOutput): Unit = {
      span .write(out)
      value.write(out)
    }

    protected def disposeData()(implicit tx: S#Tx): Unit = disconnect()

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

  implicit def entrySer[S <: Sys[S], A <: Elem[S]]: Serializer[S#Tx, S#Acc, Entry[S, A]] =
    anyEntrySer.asInstanceOf[EntrySer[S, A]]

  private val anyEntrySer = new EntrySer[NoSys, Obj[NoSys]]

  private def readEntry[S <: Sys[S], A <: Elem[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                                 (implicit tx: S#Tx): EntryImpl[S, A] = {
    val span  = SpanLikeObj.read(in, access)
    val value = Elem       .read(in, access).asInstanceOf[A]
    new EntryImpl(targets, span, value)
  }

  private final class EntrySer[S <: Sys[S], A <: Elem[S]] extends ObjSerializer[S, Entry[S, A]] {
    def tpe: Obj.Type = Entry
  }

  final def copyTree[In <: Sys[In], Out <: Sys[Out], E[~ <: Sys[~]] <: Elem[~], Repr <: Impl[Out, E, Repr]](
      in: TreeImpl[In, E], out: TreeImpl[Out, E], outImpl: Repr)
     (implicit txIn: In#Tx, txOut: Out#Tx, context: Copy[In, Out]): Unit = {

    type EntryAux[~ <: Sys[~]] = Entry[~, E[~]]
    in.iterator.foreach { case (span, xsIn) =>
      val xsOut: Vec[EntryAux[Out]] = xsIn.map(entry => context[EntryAux](entry))
      out.add(span -> xsOut)
      xsOut.foreach { entry =>
        outImpl.changed += entry
      }
    }
  }

  abstract class Impl[S <: Sys[S], E[~ <: Sys[~]] <: Elem[~], Repr <: BiGroup.Modifiable[S, E[S]]]
    extends Modifiable[S, E[S]]
    with evt.impl.SingleNode[S, BiGroup.Update[S, E[S], Repr]] {

    group: Repr =>

    type A = E[S]

    // ---- abstract ----

    implicit final def pointView: (Leaf[S, A], S#Tx) => LongPoint2DLike = (tup, _ /* tx */) => spanToPoint(tup._1)

    protected def tree: TreeImpl[S, E]

    // ---- implemented ----

    // DO NOT DEFINE THE TYPE, BECAUSE THIS IS
    // USED AS A MIXIN, E.G. BY TIMELINE
    // final def tpe: Obj.Type = BiGroup

    final def isEmpty (implicit tx: S#Tx): Boolean = tree.isEmpty
    final def nonEmpty(implicit tx: S#Tx): Boolean = !isEmpty

    final def treeHandle: TreeImpl[S, E] = tree

    // Note: must be after `EntrySer`
    protected final def newTree()(implicit tx: S#Tx): TreeImpl[S, E] =
      SkipOctree.empty[S, TwoDim, LeafImpl[S, E]](BiGroup.MaxSquare)

    // Note: must be after `EntrySer`
    protected final def readTree(in: DataInput, access: S#Acc)(implicit tx: S#Tx): TreeImpl[S, E] =
      SkipOctree.read[S, TwoDim, LeafImpl[S, E]](in, access)

    // ---- event behaviour ----

    object changed extends Changed with evt.impl.Generator[S, BiGroup.Update[S, A, Repr]]
      with evt.Caching {

      def += (entry: Entry[S, A])(implicit tx: S#Tx): Unit = entry.changed ---> this
      def -= (entry: Entry[S, A])(implicit tx: S#Tx): Unit = entry.changed -/-> this

      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[BiGroup.Update[S, A, Repr]] = {
        if (pull.isOrigin(this)) return Some(pull.resolve)

        val par = pull.parents(this)
        log(s"$this.pullUpdate -> parents = $par")

        val changes: List[BiGroup.Moved[S, A]] = par.iterator.flatMap { evt =>
          val entry = evt.node.asInstanceOf[Entry[S, A]]
          val ch0   = pull(entry.changed)
          log(s"$this.pullUpdate -> from entry $entry pulled $ch0")
          ch0.map {
            case ch @ Change(spanValOld, spanValNew) =>
              assert(removeNoFire(spanValOld, entry))
              addNoFire          (spanValNew, entry)
              BiGroup.Moved(ch, entry)
          }
        } .toList

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
            entry => BiGroup.Removed(spanVal, entry)
          }
      }
      tree.clear()
      if (changes.nonEmpty) changed.fire(BiGroup.Update(group, changes))
    }

    final def add(span: SpanLikeObj[S], elem: A)(implicit tx: S#Tx): Entry[S, A] = {
      log(s"$this.add($span, $elem)")
      val spanVal = span.value
      val tgt     = evt.Targets[S]
      val entry   = new EntryImpl[S, A](tgt, span, elem).connect()
      addNoFire(spanVal, entry)

      changed += entry
      changed.fire(BiGroup.Update(group, BiGroup.Added(spanVal, entry) :: Nil))

      entry
    }

    private def addNoFire(spanVal: SpanLike, entry: Entry[S, A])(implicit tx: S#Tx): Unit = {
      val point = spanToPoint(spanVal)
      //if( showLog ) println( "add at point " + point )
      //         val entry   = (span, elem)
      tree.transformAt(point) {
        case None           => Some(spanVal -> Vector (entry))
        case Some((_, seq)) => Some(spanVal -> (seq :+ entry))
      }
    }

    final def recoverSpan(spanVal: SpanLike, elem: A)(implicit tx: S#Tx): Option[SpanLikeObj[S]] = {
      val point = spanToPoint(spanVal)
      tree.get(point).flatMap { case (_, vec) =>
        vec.collectFirst {
          case e if e.value === elem => e.span
        }
      }
    }

    final def get(spanVal: SpanLike)(implicit tx: S#Tx): Vec[BiGroup.Entry[S, A]] = {
      val point = spanToPoint(spanVal)
      tree.get(point).fold[Vec[BiGroup.Entry[S, A]]](Vector.empty)(_._2)
    }

    final def remove(span: SpanLikeObj[S], elem: A)(implicit tx: S#Tx): Boolean = {
      val spanVal   = span.value
      val point     = spanToPoint(spanVal)
      val entryOpt  = tree.get(point).flatMap {
        case (_, Vec(single)) =>
          if (single.span === span && single.value === elem) {
            tree.removeAt(point)
            Some(single)
          } else {
            None
          }
        case (_, seq) =>
          val (equal, diff) = seq.partition(entry => entry.span === span && entry.value === elem)
          if (equal.nonEmpty) {
            tree.add((spanVal, diff))
            equal.headOption
          } else {
            None
          }
      }

      entryOpt.foreach { entry =>
        changed -= entry
        changed.fire(BiGroup.Update(group, BiGroup.Removed(spanVal, entry) :: Nil))
        entry.dispose()
      }

      entryOpt.isDefined
    }

    private def removeNoFire(spanVal: SpanLike, entry: Entry[S, A])(implicit tx: S#Tx): Boolean = {
      val point = spanToPoint(spanVal)
      val found = tree.get(point)
      found match {
        case Some((_, Vec(single))) =>
          if (single === entry) {
            assert(tree.removeAt(point).isDefined)
            true
          } else {
            false
          }
        case Some((_, seq)) =>
          val seqNew = seq.filterNot(_ === entry)
          if (seqNew.size !== seq.size) {
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

    final def iterator(implicit tx: S#Tx): Iterator[Leaf[S, A]] = tree.iterator

    final def intersect(time: Long)(implicit tx: S#Tx): Iterator[Leaf[S, A]] =
      BiGroupImpl.intersectTime(tree)(time)

    final def intersect(span: SpanLike)(implicit tx: S#Tx): Iterator[Leaf[S, A]] =
      BiGroupImpl.intersectSpan(tree)(span)

    final def rangeSearch(start: SpanLike, stop: SpanLike)(implicit tx: S#Tx): Iterator[Leaf[S, A]] =
      BiGroupImpl.rangeSearch(tree)(start, stop)

    // this can be easily implemented with two rectangular range searches
    final def eventsAt(time: Long)(implicit tx: S#Tx): (Iterator[Leaf[S, A]], Iterator[Leaf[S, A]]) =
      BiGroupImpl.eventsAt(tree)(time)

    final def eventAfter(time: Long)(implicit tx: S#Tx): Option[Long] =
      BiGroupImpl.eventAfter(tree)(time)

    final def eventBefore(time: Long)(implicit tx: S#Tx): Option[Long] =
      BiGroupImpl.eventBefore(tree)(time)

    final def firstEvent(implicit tx: S#Tx): Option[Long] =
      BiGroupImpl.eventAfter(tree)(BiGroup.MinCoordinate)

    final def lastEvent(implicit tx: S#Tx): Option[Long] =
      BiGroupImpl.eventBefore(tree)(BiGroup.MaxCoordinate)

    protected type GroupAux[~ <: Sys[~]] = BiGroup[~, E[~]]
  }

  def newModifiable[S <: Sys[S], E[~ <: Sys[~]] <: Elem[~]](implicit tx: S#Tx): Modifiable[S, E[S]] =
    new Impl1[S, E](Targets[S]) {
      val tree: TreeImpl[S, E] = newTree()
    }

  private def read[S <: Sys[S], E[~ <: Sys[~]] <: Elem[~]](in: DataInput, access: S#Acc, _targets: evt.Targets[S])
                                            (implicit tx: S#Tx): Impl[S, E, Impl1[S, E]] =
    new Impl1[S, E](_targets) {
      val tree: TreeImpl[S, E] = readTree(in, access)
    }

  private abstract class Impl1[S <: Sys[S], E[~ <: Sys[~]] <: Elem[~]](protected val targets: Targets[S])
    extends Impl[S, E, Impl1[S, E]] { in =>

    final def tpe: Obj.Type = BiGroup

    override def toString: String = s"BiGroup${tree.id}"

    def modifiableOption: Option[BiGroup.Modifiable[S, A]] = Some(this)

    private[lucre] def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
      new Impl1[Out, E](Targets[Out]) { out =>
        val tree: TreeImpl[Out, E] = out.newTree()
        context.defer[GroupAux](in, out)(copyTree[S, Out, E, Impl1[Out, E]](in.tree, out.tree, out))
        // connect()
      }
  }
}