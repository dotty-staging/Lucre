/*
 *  ExpandedMapExSeqOrOption.scala
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

package de.sciss.lucre.expr.graph.impl

import de.sciss.lucre.event.impl.IChangeEventImpl
import de.sciss.lucre.event.{Caching, IChangeEvent, IPull, IPush, ITargets}
import de.sciss.lucre.expr.graph.{Ex, It}
import de.sciss.lucre.expr.{Context, IExpr}
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.stm.{Disposable, Sys}

import scala.collection.mutable
import scala.concurrent.stm.Ref

abstract class ExpandedMapSeqOrOption[S <: Sys[S], A, In[_], P, Out](in: IExpr[S, In[A]], it: It.Expanded[S, A],
                                                                     /* closure: Graph, */ fun: Ex[P], tx0: S#Tx)
                                                                    (implicit protected val targets: ITargets[S], ctx: Context[S])
  extends IExpr[S, Out] with IChangeEventImpl[S, Out] with Caching {

  protected type Tuples = In[(IExpr[S, P], Disposable[S#Tx])]

  // ---- abstract: in ----

  protected def isEmpty[A1](in: In[A1]): Boolean

  protected def foreach[A1](in: In[A1])(body: A1 => Unit): Unit

  protected def emptyIn[A1]: In[A1]

  protected def emptyOut: Out

  protected def map[A1, B1](in: In[A1])(body: A1 => B1): In[B1]

  protected def buildResult(inV: In[A], tuples: Tuples)(elem: IExpr[S, P] => P)(implicit tx: S#Tx): Out

  // ---- impl ----

  private val ref = Ref.make[Tuples]()

  private def init()(implicit tx: S#Tx): Unit = {
    in .changed.--->(this)
    val inV   = in.value
    val refV  = mkRef(inV)
    ref()     = refV
  }

  init()(tx0)

  private def disposeRef(refV: In[(IExpr[S, P], Disposable[S#Tx])])(implicit tx: S#Tx): Unit =
    foreach(refV) { case (f, d) =>
      f.changed -/-> this
      d.dispose()
    }

  private[lucre] def pullChange(pull: IPull[S])(implicit tx: S#Tx, phase: IPull.Phase): Out = {
    val inV = pull.expr(in)
    if (pull.contains(in.changed) && phase.isNow) {
      val refV = mkRef(inV)
      disposeRef(ref.swap(refV))
    }
    if (isEmpty(inV)) emptyOut
    else {
      buildResult(inV, ref())(pull.expr(_))
    }
  }

  private def mkRef(inV: In[A])(implicit tx: S#Tx): Tuples =
    map(inV) { v =>
      it.setValue(v)
      val (f, d) = ctx.nested(it) {
        val _f = fun.expand[S]
        _f.changed ---> this
        _f
      }
      (f, d)
    }

  final def value(implicit tx: S#Tx): Out =
    IPush.tryPull(this).fold({
      val inV = in.value
      if (isEmpty(inV)) emptyOut
      else {
        buildResult(inV, ref())(_.value)
      }
    })(_.now)

  final def dispose()(implicit tx: S#Tx): Unit = {
    in .changed.-/->(this)
    disposeRef(ref.swap(emptyIn))
  }

  def changed: IChangeEvent[S, Out] = this
}

// sequence input

abstract class ExpandedMapSeqIn[S <: Sys[S], A, P, Out](in: IExpr[S, Seq[A]], it: It.Expanded[S, A],
                                                         /* closure: Graph, */ fun: Ex[P], tx0: S#Tx)
                                                        (implicit targets: ITargets[S], ctx: Context[S])
  extends ExpandedMapSeqOrOption[S, A, Seq, P, Out](in, it, fun, tx0) {

  // ---- impl ----

  protected final def isEmpty[A1](in: Seq[A1]): Boolean = in.isEmpty

  protected final def foreach[A1](in: Seq[A1])(body: A1 => Unit): Unit = in.foreach(body)

  protected final def emptyIn[A1]: Seq[A1] = Nil

  protected final def map[A1, B1](in: Seq[A1])(body: A1 => B1): Seq[B1] = in.map(body)
}

abstract class ExpandedMapSeqLike[S <: Sys[S], A, P, B](in: IExpr[S, Seq[A]], it: It.Expanded[S, A],
                                                          /* closure: Graph, */ fun: Ex[P], tx0: S#Tx)
                                                         (implicit targets: ITargets[S], ctx: Context[S])
  extends ExpandedMapSeqIn[S, A, P, Seq[B]](in, it, fun, tx0) {

  // ---- abstract: out ----

  protected def append(b: mutable.Builder[B, _], cc: P): Unit

  // ---- impl ----

  protected final def emptyOut: Seq[B] = Nil

  protected final def buildResult(inV: Seq[A], tuples: Tuples)(elem: IExpr[S, P] => P)
                                 (implicit tx: S#Tx): Seq[B] = {
    if (tuples.size != inV.size) {
      val err = new AssertionError(s"inV.size = ${inV.size}, tuples.size = ${tuples.size} in $this")
//      err.fillInStackTrace()
//      Console.err.println(s"Assertion failed in $this")
//      err.printStackTrace()
      throw err
    }
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

  override def toString: String = s"$in.flatMap($fun)"

  protected def append(b: mutable.Builder[B, _], cc: Seq[B]): Unit =
    b ++= cc
}

final class ExpandedFlatMapSeqOption[S <: Sys[S], A, B](in: IExpr[S, Seq[A]], it: It.Expanded[S, A],
                                                        /* closure: Graph, */ fun: Ex[Option[B]], tx0: S#Tx)
                                                       (implicit targets: ITargets[S], ctx: Context[S])
  extends ExpandedMapSeqLike[S, A, Option[B], B](in, it, fun, tx0) {

  override def toString: String = s"$in.flatMap($fun)"

  protected def append(b: mutable.Builder[B, _], cc: Option[B]): Unit = cc match {
    case Some(c)  => b += c
    case None     =>
  }
}

// option input

abstract class ExpandedMapOptionLike[S <: Sys[S], A, P, B](in: IExpr[S, Option[A]], it: It.Expanded[S, A],
                                                         /* closure: Graph, */ fun: Ex[P], tx0: S#Tx)
                                                        (implicit targets: ITargets[S], ctx: Context[S])
  extends ExpandedMapSeqOrOption[S, A, Option, P, Option[B]](in, it, fun, tx0) {

  // ---- impl ----

  protected final def isEmpty[A1](in: Option[A1]): Boolean = in.isEmpty

  protected final def foreach[A1](in: Option[A1])(body: A1 => Unit): Unit = in.foreach(body)

  protected final def emptyIn[A1]: Option[A1] = None

  protected final def emptyOut: Option[B] = None

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