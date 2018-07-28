/*
 *  Ex.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2018 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr

import de.sciss.lucre.aux.ProductWithAux
import de.sciss.lucre.event.ITargets
import de.sciss.lucre.expr.Ex.Context
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Obj, Sys}

object Ex {
//  object Var {
//    private final class Expanded[S <: Sys[S], A](init: IExpr[S, A], tx0: S#Tx)
//                                                (implicit protected val targets: ITargets[S])
//      extends IExpr.Var[S, A] with IGenerator[S, Change[A]] {
//
//      private[this] val ref = Ref(init)
//
//      init.changed.--->(this)(tx0)
//
//      def value(implicit tx: S#Tx): A = apply().value
//
//      def apply()(implicit tx: S#Tx): IExpr[S, A] = ref.get(tx.peer)
//
//      def update(v: IExpr[S, A])(implicit tx: S#Tx): Unit = {
//        import TxnLike.peer
//        val before = ref()
//        if (before != v) {
//          before.changed -/-> this
//          ref() = v
//          v     .changed ---> this
//
//          val beforeV = before.value
//          val exprV   = v     .value
//          fire(Change(beforeV, exprV))
//        }
//      }
//
//      def dispose()(implicit tx: S#Tx): Unit = {
//        import TxnLike.peer
//        ref().changed -/-> changed
//      }
//
//      private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Change[A]] = {
////        if (pull.parents(this).isEmpty) {
//          Some(pull.resolve[Change[A]])
////        } else {
////          pull(ref().changed)
////        }
//      }
//
//      def changed: IEvent[S, Change[A]] = this
//    }
//  }
//
//  final case class Var[A](init: Ex[A]) extends Ex[A] {
//    // this acts now as a fast unique reference
//    @transient final private[this] lazy val ref = new AnyRef
//
//    override def expand[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): IExpr.Var[S, A] = {
//      import ctx.targets
//      val initEx = init.expand[S]
//      ctx.visit(ref, new Var.Expanded[S, A](initEx, tx))
//    }
//
//    def aux: scala.List[Aux] = Nil
//  }
//
//  trait Var[A] extends Ex[A] {
//    def <--> (attr: ExAttrLike[A]): Unit
//  }

  object Context {
    def apply[S <: Sys[S]](selfH: Option[stm.Source[S#Tx, Obj[S]]] = None): Context[S] =
      new impl.ContextImpl[S](selfH)
  }
  trait Context[S <: Sys[S]] {
    implicit def targets: ITargets[S]

    def selfOption(implicit tx: S#Tx): Option[Obj[S]]

    def visit[U](ref: AnyRef, init: => U)(implicit tx: S#Tx): U
  }

  trait Lazy[A] extends Ex[A] {
    // this acts now as a fast unique reference
    @transient final private[this] lazy val ref = new AnyRef

    final def expand[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): IExpr[S, A] =
      ctx.visit(ref, mkExpr)

    protected def mkExpr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): IExpr[S, A]
  }
}
trait Ex[+A] extends ProductWithAux {
  def expand[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): IExpr[S, A]
}
