/*
 *  Expr.scala
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

import de.sciss.lucre.event.Publisher
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.lucre.stm
import de.sciss.model.Change

object Expr {
  trait Var[S <: Sys[S], A] extends Expr[S, A] with stm.Var[S#Tx, Expr[S, A]]

  object Const {
    def unapply[S <: Sys[S], A](expr: Expr[S, A]): Option[A] =
      if (expr   .isInstanceOf[Const[_, _]]) {
        Some(expr.asInstanceOf[Const[S, A]].constValue)
      } else None
  }

  /** A constant expression simply acts as a proxy for a constant value of type `A`.
    * Its event is a dummy (constants never change), and the `value` method does
    * not need to use the transaction. String representation, hash-code and equality
    * are defined in terms of the constant peer value.
    */
  trait Const[S <: Sys[S], +A] extends Expr[S, A] {
    protected def constValue: A
  }

  def isConst(expr: Expr[_, _]): Boolean = expr.isInstanceOf[Const[_, _]]
}

/** An expression is a computation that reduces to a single value of type `A`.
  * Expressions can be understood as dataflow variables. When a tree is
  * composed, a change in the root of the tree propagates through to the leaves
  * in the form of an emitted `Change` event that carries the old and new
  * value (according to the particular node of the tree).
  *
  * Basic expression types are `Expr.Const` - it simply wraps a constant value
  * and thus will never change or fire an event - and `Expr.Var` which can be
  * thought of as a mutable variable carrying a peer expression. When the variable
  * assignment changes, the expression currently held is evaluated and propagated
  * as an event. Intermediate nodes or expressions might modify the value, such
  * as a binary operator (e.g., an integer expression that sums two input
  * integer expressions).
  */
trait Expr[S <: Sys[S], +A] extends ExprLike[S, A] with Obj[S] with Publisher[S, Change[A]]