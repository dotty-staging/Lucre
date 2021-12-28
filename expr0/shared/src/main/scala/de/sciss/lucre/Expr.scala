/*
 *  Expr.scala
 *  (Lucre 4)
 *
 *  Copyright (c) 2009-2021 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre

import de.sciss.lucre
import de.sciss.lucre.expr.graph.Ex
import de.sciss.lucre.expr.impl.{ExObjBridgeImpl, ExSeqObjBridgeImpl}
import de.sciss.model.Change
import de.sciss.serial.{ConstFormat, DataInput, TFormat}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.language.implicitConversions

object Expr /*extends expr.Ops*/ {
  // XXX TODO -- we need to rethink this type
  //  trait Var[T <: Txn[T], A, E[~ <: Txn[~]] <: Expr[~, A]] extends Expr[S, A] with stm.Var[T, E[T]]
  //  trait Var[T <: Txn[T], A] extends Expr[T, A] with lucre.Var[Expr[T, A]]

  object Const {
    def unapply[T <: Txn[T], A](expr: Expr[T, A]): Option[A] =
      if (expr   .isInstanceOf[Const[_, _]]) {
        Some(expr.asInstanceOf[Const[T, A]].constValue)
      } else None
  }

  /** A constant expression simply acts as a proxy for a constant value of type `A`.
   * Its event is a dummy (constants never change), and the `value` method does
   * not need to use the transaction. String representation, hash-code and equality
   * are defined in terms of the constant peer value.
   */
  trait Const[T <: Txn[T], +A] extends Expr[T, A] {
    protected def constValue: A
  }

  object Program {
    def unapply[T <: Txn[T], A](expr: Expr[T, A]): Option[Ex[A]] =
      if (expr   .isInstanceOf[Program[_, _]]) {
        Some(expr.asInstanceOf[Program[T, A]].program)
      } else None
  }

  /** A program expression is backed by a code tree with an `Ex` as its leaf.
    * Program inputs are accessed through the object's attribute map.
    */
  trait Program[T <: Txn[T], +A] extends Expr[T, A] {
    protected def program: Ex[A]
  }

  def isConst   (expr: Expr[_, _]): Boolean = expr.isInstanceOf[Const   [_, _]]
  def isProgram (expr: Expr[_, _]): Boolean = expr.isInstanceOf[Program [_, _]]

  object Type {
    private lazy val _init: Unit = {
      Adjunct.addFactory(Type.ObjBridge)
      Adjunct.addFactory(Type.SeqObjBridge)
    }

    def init(): Unit = _init

    object ObjBridge extends Adjunct.Factory {
      final val id = 1000

      def readIdentifiedAdjunct(in: DataInput): Adjunct = {
        val typeId  = in.readInt()
        val peer    = Obj.getType(typeId)
        val peerT   = peer.asInstanceOf[Expr.Type[scala.Any, Expr.Any]]
        new ExObjBridgeImpl[scala.Any, Expr.Any](peerT)
      }
    }

    object SeqObjBridge extends Adjunct.Factory {
      final val id = 1010

      def readIdentifiedAdjunct(in: DataInput): Adjunct = {
        val typeId  = in.readInt()
        val peer    = Obj.getType(typeId)
        val peerT   = peer.asInstanceOf[Expr.Type[Vec[scala.Any], Expr.AnyVec]]
        new ExSeqObjBridgeImpl[scala.Any, Expr.AnyVec](peerT)
      }
    }
  }

  trait Type[A1, Repr[~ <: Txn[~]] <: Expr[~, A1]] extends Obj.Type {
    type A = A1
    type E[T <: Txn[T]] = Repr[T] // yeah, well, we're waiting for Dotty
    // N.B.: this causes trouble:
    //     type Var  [T <: Txn[T]] = Repr[T] with _Expr.Var  [S, A, _Ex]
    type Var    [T <: Txn[T]] = Repr[T] with lucre.Var    [T, Repr[T]]
    type Const  [T <: Txn[T]] = Repr[T] with Expr.Const   [T, A]
    type Program[T <: Txn[T]] = Repr[T] with Expr.Program [T, A]

    // ---- abstract ----

    def read[T <: Txn[T]](in: DataInput)(implicit tx: T): Repr[T]

    implicit def format   [T <: Txn[T]]: TFormat[T, Repr[T]]
    implicit def varFormat[T <: Txn[T]]: TFormat[T, Var [T]]

    implicit def valueFormat: ConstFormat[A]

    // ---- public ----

    object Var {
      def unapply[T <: Txn[T]](expr: E[T]): Option[Var[T]] = {
        // !!! this wrongly reports `true` for `Const`, probably due
        // to some erasure that scalac doesn't warn about
        // if (expr.isInstanceOf[Var[_]]) Some(expr.asInstanceOf[Var[T]]) else None

        if (expr.isInstanceOf[lucre.Var[T, _]]) Some(expr.asInstanceOf[Var[T]]) else None
      }
    }

    implicit def newConst [T <: Txn[T]](value: A     )(implicit tx: T): Const[T]
    def newVar            [T <: Txn[T]](init: Repr[T])(implicit tx: T): Var  [T]

    def readConst[T <: Txn[T]](in: DataInput)(implicit tx: T): Const[T]
    def readVar  [T <: Txn[T]](in: DataInput)(implicit tx: T): Var  [T]

    def tryParse(value: scala.Any): Option[A]
  }

  // work-around for https://github.com/lampepfl/dotty/issues/11464
  private type Any    [T <: Txn[T]] = ({ type R[~ <: Txn[~]] <: Expr[~,     scala.Any ] }) # R[T]
  private type AnyVec [T <: Txn[T]] = ({ type R[~ <: Txn[~]] <: Expr[~, Vec[scala.Any]] }) # R[T]
}

/** An expression is a computation that reduces to a single value of type `A`.
 * Expressions can be understood as data-flow variables. When a tree is
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
trait Expr[T <: Txn[T], +A] extends ExprLike[T, A] with Obj[T] with Publisher[T, Change[A]]
