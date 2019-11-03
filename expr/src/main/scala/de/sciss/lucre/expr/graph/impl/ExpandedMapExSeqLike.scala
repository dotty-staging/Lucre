/*
 *  ExpandedMapExSeq.scala
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

import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.event.impl.IChangeEventImpl
import de.sciss.lucre.event.{Caching, IChangeEvent, IPull, ITargets}
import de.sciss.lucre.expr.{Context, IExpr}
import de.sciss.lucre.expr.graph.{Ex, It}
import de.sciss.lucre.stm.{Disposable, Sys}

import scala.collection.mutable
import scala.concurrent.stm.Ref

abstract class ExpandedMapExSeqLike[S <: Sys[S], A, CC, B](in: IExpr[S, Seq[A]], it: It.Expanded[S, A],
                                                       /* closure: Graph, */ fun: Ex[CC], tx0: S#Tx)
                                                      (implicit protected val targets: ITargets[S], ctx: Context[S])
  extends IExpr[S, Seq[B]] with IChangeEventImpl[S, Seq[B]] with Caching {

  protected def append(b: mutable.Builder[B, _], cc: CC): Unit

  private type Tuples = Seq[(IExpr[S, CC], Disposable[S#Tx])]

  private[this] val ref = Ref.make[Tuples]

  private def init()(implicit tx: S#Tx): Unit = {
    in .changed.--->(this)
    //  fun.changed.--->(this)(tx0)
    // this `it` is in the push path and we can wipe the cache
//    in. changed.--->(it.changed)

    val inV   = in.value
    val refV  = mkRef(inV)
    ref()     = refV
//    disposeRef(ref.swap(refV))
  }

  init()(tx0)

  private def mkRef(inV: Seq[A])(implicit tx: S#Tx): Tuples =
    inV.map { v =>
      it.setValue(v)
      val (f, d) = ctx.nested {
        val _f = fun.expand[S]
        _f.changed ---> this
        _f
      }
      (f, d)
    }

  private def disposeRef(refV: Seq[(IExpr[S, CC], Disposable[S#Tx])])(implicit tx: S#Tx): Unit =
    refV.foreach { case (f, d) =>
      f.changed -/-> this
      d.dispose()
    }

  def value(implicit tx: S#Tx): Seq[B] = {
    val inV = in.value
    if (inV.isEmpty) Nil
    else {
      buildResult(inV)(_.value)
    }
  }

  private def buildResult(inV: Seq[A])(elem: IExpr[S, CC] => CC)
                         (implicit tx: S#Tx): Seq[B] = {
    val tuples    = ref()
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

  private[lucre] def pullChange(pull: IPull[S])(implicit tx: S#Tx, phase: IPull.Phase): Seq[B] = {
    val inV = pull.expr(in)
    if (inV.isEmpty) Nil
    else /*pull.nonCached(it.changed)*/ {
      if (pull.contains(in.changed) && phase.isNow) {
        val refV = mkRef(inV)
        disposeRef(ref.swap(refV))
      }
      buildResult(inV)(pull.expr(_))
    }
  }

  def dispose()(implicit tx: S#Tx): Unit = {
    in .changed.-/->(this)
//    fun.changed.-/->(this)
//    in. changed.-/->(it.changed)
    disposeRef(ref.swap(Nil))
  }

  def changed: IChangeEvent[S, Seq[B]] = this
}

final class ExpandedMapExSeq[S <: Sys[S], A, B](in: IExpr[S, Seq[A]], it: It.Expanded[S, A],
                                                /* closure: Graph, */ fun: Ex[B], tx0: S#Tx)
                                               (implicit targets: ITargets[S], ctx: Context[S])
  extends ExpandedMapExSeqLike[S, A, B, B](in, it, fun, tx0) {

  override def toString: String = s"$in.map($fun)"

  protected def append(b: mutable.Builder[B, _], cc: B): Unit = b += cc
}

final class ExpandedFlatMapExSeq[S <: Sys[S], A, B](in: IExpr[S, Seq[A]], it: It.Expanded[S, A],
                                                    /* closure: Graph, */ fun: Ex[Seq[B]], tx0: S#Tx)
                                                   (implicit targets: ITargets[S], ctx: Context[S])
  extends ExpandedMapExSeqLike[S, A, Seq[B], B](in, it, fun, tx0) {

  protected def append(b: mutable.Builder[B, _], cc: Seq[B]): Unit =
    b ++= cc
}

final class ExpandedFlatMapExSeqOption[S <: Sys[S], A, B](in: IExpr[S, Seq[A]], it: It.Expanded[S, A],
                                                          /* closure: Graph, */ fun: Ex[Option[B]], tx0: S#Tx)
                                                         (implicit targets: ITargets[S], ctx: Context[S])
  extends ExpandedMapExSeqLike[S, A, Option[B], B](in, it, fun, tx0) {

  protected def append(b: mutable.Builder[B, _], cc: Option[B]): Unit = cc match {
    case Some(c)  => b += c
    case None     =>
  }
}
