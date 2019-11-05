/*
 *  ExpandedMapExSeqOrOption.scala
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

package de.sciss.lucre.expr.graph.impl

import de.sciss.lucre.event.impl.IChangeEventImpl
import de.sciss.lucre.event.{Caching, IChangeEvent, IPull, ITargets}
import de.sciss.lucre.expr.graph.{Ex, It}
import de.sciss.lucre.expr.{Context, IExpr}
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.stm.{Disposable, Sys}

import scala.collection.mutable
import scala.concurrent.stm.Ref
import scala.language.higherKinds

abstract class ExpandedMapSeqOrOption[S <: Sys[S], A, CA[_], CB, B](in: IExpr[S, CA[A]], it: It.Expanded[S, A],
                                                                    /* closure: Graph, */ fun: Ex[CB], tx0: S#Tx)
                                                                   (implicit protected val targets: ITargets[S], ctx: Context[S])
  extends IExpr[S, CA[B]] with IChangeEventImpl[S, CA[B]] with Caching {

  protected type Tuples = CA[(IExpr[S, CB], Disposable[S#Tx])]

  // ---- abstract: in ----

  protected def isEmpty[A1](in: CA[A1]): Boolean

  protected def foreach[A1](in: CA[A1])(body: A1 => Unit): Unit

  protected def empty[A1]: CA[A1]

  protected def map[A1, B1](in: CA[A1])(body: A1 => B1): CA[B1]

  protected def buildResult(inV: CA[A], tuples: Tuples)(elem: IExpr[S, CB] => CB)(implicit tx: S#Tx): CA[B]

  // ---- impl ----

  private val ref = Ref.make[Tuples]

  private def init()(implicit tx: S#Tx): Unit = {
    in .changed.--->(this)
    val inV   = in.value
    val refV  = mkRef(inV)
    ref()     = refV
  }

  init()(tx0)

  private def disposeRef(refV: CA[(IExpr[S, CB], Disposable[S#Tx])])(implicit tx: S#Tx): Unit =
    foreach(refV) { case (f, d) =>
      f.changed -/-> this
      d.dispose()
    }

  private[lucre] def pullChange(pull: IPull[S])(implicit tx: S#Tx, phase: IPull.Phase): CA[B] = {
    val inV = pull.expr(in)
    if (isEmpty(inV)) empty
    else /*pull.nonCached(it.changed)*/ {
      if (pull.contains(in.changed) && phase.isNow) {
        val refV = mkRef(inV)
        disposeRef(ref.swap(refV))
      }
      buildResult(inV, ref())(pull.expr(_))
    }
  }

  private def mkRef(inV: CA[A])(implicit tx: S#Tx): Tuples =
    map(inV) { v =>
      it.setValue(v)
      val (f, d) = ctx.nested(it) {
        val _f = fun.expand[S]
        _f.changed ---> this
        _f
      }
      (f, d)
    }

  final def value(implicit tx: S#Tx): CA[B] = {
    val inV = in.value
    if (isEmpty(inV)) empty
    else {
      buildResult(inV, ref())(_.value)
    }
  }

  final def dispose()(implicit tx: S#Tx): Unit = {
    in .changed.-/->(this)
    disposeRef(ref.swap(empty))
  }

  def changed: IChangeEvent[S, CA[B]] = this
}

// sequence input

abstract class ExpandedMapSeqLike[S <: Sys[S], A, CC, B](in: IExpr[S, Seq[A]], it: It.Expanded[S, A],
                                                         /* closure: Graph, */ fun: Ex[CC], tx0: S#Tx)
                                                        (implicit targets: ITargets[S], ctx: Context[S])
  extends ExpandedMapSeqOrOption[S, A, Seq, CC, B](in, it, fun, tx0) {

  // ---- abstract: out ----

  protected def append(b: mutable.Builder[B, _], cc: CC): Unit

  // ---- impl ----

  protected final def isEmpty[A1](in: Seq[A1]): Boolean = in.isEmpty

  protected final def foreach[A1](in: Seq[A1])(body: A1 => Unit): Unit = in.foreach(body)

  protected final def empty[A1]: Seq[A1] = Nil

  protected final def map[A1, B1](in: Seq[A1])(body: A1 => B1): Seq[B1] = in.map(body)

  protected final def buildResult(inV: Seq[A], tuples: Tuples)(elem: IExpr[S, CC] => CC)
                         (implicit tx: S#Tx): Seq[B] = {
    assert (tuples.size == inV.size)
    val iterator  = inV.iterator zip tuples.iterator
    val b         = Seq.newBuilder[B]
    b.sizeHint(inV)
    while (iterator.hasNext) {
      val (vn, (f, _)) = iterator.next()
      it.setValue(vn /*, dispatch = true*/)
      val funV = elem(f)
      append(b, funV)
    }
    b.result()
  }
}

final class ExpandedMapSeq[S <: Sys[S], A, B](in: IExpr[S, Seq[A]], it: It.Expanded[S, A],
                                              /* closure: Graph, */ fun: Ex[B], tx0: S#Tx)
                                             (implicit targets: ITargets[S], ctx: Context[S])
  extends ExpandedMapSeqLike[S, A, B, B](in, it, fun, tx0) {

  override def toString: String = s"$in.map($fun)"

  protected def append(b: mutable.Builder[B, _], cc: B): Unit = b += cc
}

final class ExpandedFlatMapSeq[S <: Sys[S], A, B](in: IExpr[S, Seq[A]], it: It.Expanded[S, A],
                                                  /* closure: Graph, */ fun: Ex[Seq[B]], tx0: S#Tx)
                                                 (implicit targets: ITargets[S], ctx: Context[S])
  extends ExpandedMapSeqLike[S, A, Seq[B], B](in, it, fun, tx0) {

  protected def append(b: mutable.Builder[B, _], cc: Seq[B]): Unit =
    b ++= cc
}

final class ExpandedFlatMapSeqOption[S <: Sys[S], A, B](in: IExpr[S, Seq[A]], it: It.Expanded[S, A],
                                                        /* closure: Graph, */ fun: Ex[Option[B]], tx0: S#Tx)
                                                       (implicit targets: ITargets[S], ctx: Context[S])
  extends ExpandedMapSeqLike[S, A, Option[B], B](in, it, fun, tx0) {

  protected def append(b: mutable.Builder[B, _], cc: Option[B]): Unit = cc match {
    case Some(c)  => b += c
    case None     =>
  }
}

// option input

abstract class ExpandedMapOptionLike[S <: Sys[S], A, CC, B](in: IExpr[S, Option[A]], it: It.Expanded[S, A],
                                                         /* closure: Graph, */ fun: Ex[CC], tx0: S#Tx)
                                                        (implicit targets: ITargets[S], ctx: Context[S])
  extends ExpandedMapSeqOrOption[S, A, Option, CC, B](in, it, fun, tx0) {

  // ---- impl ----

  protected final def isEmpty[A1](in: Option[A1]): Boolean = in.isEmpty

  protected final def foreach[A1](in: Option[A1])(body: A1 => Unit): Unit = in.foreach(body)

  protected final def empty[A1]: Option[A1] = None

  protected final def map[A1, B1](in: Option[A1])(body: A1 => B1): Option[B1] = in.map(body)
}

final class ExpandedMapOption[S <: Sys[S], A, B](in: IExpr[S, Option[A]], it: It.Expanded[S, A],
                                                 fun: Ex[B], tx0: S#Tx)
                                                (implicit targets: ITargets[S], ctx: Context[S])
  extends ExpandedMapOptionLike[S, A, B, B](in, it, fun, tx0) {

  override def toString: String = s"$in.map($fun)"

  protected def buildResult(inV: Option[A], tuples: Tuples)(elem: IExpr[S, B] => B)
                           (implicit tx: S#Tx): Option[B] =
    inV.flatMap { vn =>
      tuples.map { case (f, _) =>
        it.setValue(vn)
        elem(f)
      }
    }
}

final class ExpandedFlatMapOption[S <: Sys[S], A, B](in: IExpr[S, Option[A]], it: It.Expanded[S, A],
                                                     fun: Ex[Option[B]], tx0: S#Tx)
                                                    (implicit targets: ITargets[S], ctx: Context[S])
  extends ExpandedMapOptionLike[S, A, Option[B], B](in, it, fun, tx0) {

  override def toString: String = s"$in.flatMap($fun)"

  protected def buildResult(inV: Option[A], tuples: Tuples)(elem: IExpr[S, Option[B]] => Option[B])
                           (implicit tx: S#Tx): Option[B] =
    inV.flatMap { vn =>
      tuples.flatMap { case (f, _) =>
        it.setValue(vn)
        elem(f)
      }
    }
}