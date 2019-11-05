///*
// *  ExpandedSeqPredicate.scala
// *  (Lucre)
// *
// *  Copyright (c) 2009-2019 Hanns Holger Rutz. All rights reserved.
// *
// *  This software is published under the GNU Affero General Public License v3+
// *
// *
// *  For further information, please contact Hanns Holger Rutz at
// *  contact@sciss.de
// */
//
//package de.sciss.lucre.expr.graph.impl
//
//import de.sciss.lucre.event.impl.IChangeEventImpl
//import de.sciss.lucre.event.{Caching, IChangeEvent, IPull, ITargets}
//import de.sciss.lucre.expr.graph.{Ex, It}
//import de.sciss.lucre.expr.{Context, IExpr}
//import de.sciss.lucre.stm.TxnLike.peer
//import de.sciss.lucre.stm.{Disposable, Sys}
//
//import scala.concurrent.stm.Ref
//import scala.language.higherKinds
//
//abstract class ExpandedSeqPredicate[S <: Sys[S], A, In[_], P, Out](in: IExpr[S, In[A]], it: It.Expanded[S, A],
//                                                                    /* closure: Graph, */ fun: Ex[P], tx0: S#Tx)
//                                                                   (implicit protected val targets: ITargets[S], ctx: Context[S])
//  extends IExpr[S, Out] with IChangeEventImpl[S, Out] with Caching {
//
//  protected type Tuples = In[(IExpr[S, P], Disposable[S#Tx])]
//
//  // ---- abstract: in ----
//
//  protected def isEmpty[A1](in: In[A1]): Boolean
//
//  protected def foreach[A1](in: In[A1])(body: A1 => Unit): Unit
//
//  protected def emptyIn[A1]: In[A1]
//
//  protected def emptyOut: Out
//
//  protected def map[A1, B1](in: In[A1])(body: A1 => B1): In[B1]
//
//  protected def buildResult(inV: In[A], tuples: Tuples)(elem: IExpr[S, P] => P)(implicit tx: S#Tx): Out
//
//  // ---- impl ----
//
//  private val ref = Ref.make[Tuples]
//
//  private def init()(implicit tx: S#Tx): Unit = {
//    in .changed.--->(this)
//    val inV   = in.value
//    val refV  = mkRef(inV)
//    ref()     = refV
//  }
//
//  init()(tx0)
//
//  private def disposeRef(refV: In[(IExpr[S, P], Disposable[S#Tx])])(implicit tx: S#Tx): Unit =
//    foreach(refV) { case (f, d) =>
//      f.changed -/-> this
//      d.dispose()
//    }
//
//  private[lucre] def pullChange(pull: IPull[S])(implicit tx: S#Tx, phase: IPull.Phase): Out = {
//    val inV = pull.expr(in)
//    if (isEmpty(inV)) emptyOut
//    else /*pull.nonCached(it.changed)*/ {
//      if (pull.contains(in.changed) && phase.isNow) {
//        val refV = mkRef(inV)
//        disposeRef(ref.swap(refV))
//      }
//      buildResult(inV, ref())(pull.expr(_))
//    }
//  }
//
//  private def mkRef(inV: In[A])(implicit tx: S#Tx): Tuples =
//    map(inV) { v =>
//      it.setValue(v)
//      val (f, d) = ctx.nested(it) {
//        val _f = fun.expand[S]
//        _f.changed ---> this
//        _f
//      }
//      (f, d)
//    }
//
//  final def value(implicit tx: S#Tx): Out = {
//    val inV = in.value
//    if (isEmpty(inV)) emptyOut
//    else {
//      buildResult(inV, ref())(_.value)
//    }
//  }
//
//  final def dispose()(implicit tx: S#Tx): Unit = {
//    in .changed.-/->(this)
//    disposeRef(ref.swap(emptyIn))
//  }
//
//  def changed: IChangeEvent[S, Out] = this
//}
