/*
 *  SpanLikeExtensions.scala
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

package de.sciss.lucre.expr

import de.sciss.lucre.event.Targets
import de.sciss.lucre.expr.impl.{Tuple2Op, Tuple1Op}
import de.sciss.lucre.stm.{Copy, Elem, Obj, Sys}
import de.sciss.lucre.{event => evt}
import de.sciss.serial.DataInput
import de.sciss.span.{Span, SpanLike}

import scala.annotation.switch
import scala.language.higherKinds

object SpanLikeExtensions {
  private[this] lazy val _init: Unit = {
    SpanLikeObj.registerExtension(SpanLikeTuple1s)
    SpanLikeObj.registerExtension(SpanLikeTuple2s)
  }

  def init(): Unit = _init

  type _Ex[S <: Sys[S]] = SpanLikeObj[S]

  def newExpr[S <: Sys[S]](start: LongObj[S], stop: LongObj[S])(implicit tx: S#Tx): _Ex[S] =
    BinaryOp.Apply(start, stop)

  def from[S <: Sys[S]](start: LongObj[S])(implicit tx: S#Tx): _Ex[S] =
    UnaryOp.From(start)

  def until[S <: Sys[S]](stop: LongObj[S])(implicit tx: S#Tx): _Ex[S] =
    UnaryOp.Until(stop)

  private[this] object SpanLikeTuple1s extends Type.Extension1[SpanLikeObj] {
    // final val arity = 1
    final val opLo: Int = UnaryOp.From .id
    final val opHi: Int = UnaryOp.Until.id

    val name = "Long-SpanLike Ops"

    def readExtension[S <: Sys[S]](opId: Int, in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                  (implicit tx: S#Tx): _Ex[S] = {
      import UnaryOp._
      val op /* : Op[_, _] */ = (opId: @switch) match {
        case From .id => From
        case Until.id => Until
        case _ => sys.error(s"Invalid operation id $opId")
      }
      op.read(in, access, targets)
    }
  }

  private[this] object SpanLikeTuple2s extends Type.Extension1[SpanLikeObj] {
    // final val arity = 2
    final val opLo: Int = BinaryOp.Apply.id
    final val opHi: Int = BinaryOp.Shift.id

    val name = "Long-SpanLike Ops"

    def readExtension[S <: Sys[S]](opId: Int, in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                  (implicit tx: S#Tx): _Ex[S] = {
      import BinaryOp._
      val op /* : Op[_, _, _, _] */ = opId /* : @switch */ match {
        case Apply.id => Apply
        case Shift.id => Shift
      }
      op.read(in, access, targets)
    }
  }

  final class Tuple1[S <: Sys[S], T1, ReprT1[~ <: Sys[~]] <: Expr[~, T1]](
      protected val targets: Targets[S], val op: Tuple1Op[SpanLike, T1, SpanLikeObj, ReprT1], val _1: ReprT1[S])
    extends impl.Tuple1[S, SpanLike, T1, SpanLikeObj, ReprT1] with SpanLikeObj[S] {

    def tpe: Obj.Type = SpanLikeObj

    private[lucre] def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
      new Tuple1(Targets[Out], op, context(_1)).connect()
  }

  final class Tuple2[S <: Sys[S], T1, ReprT1[~ <: Sys[~]] <: Expr[~, T1],
                                  T2, ReprT2[~ <: Sys[~]] <: Expr[~, T2]](
      protected val targets: Targets[S], val op: Tuple2Op[SpanLike, T1, T2, SpanLikeObj, ReprT1, ReprT2],
      val _1: ReprT1[S], val _2: ReprT2[S])
    extends impl.Tuple2[S, SpanLike, T1, T2, SpanLikeObj, ReprT1, ReprT2] with SpanLikeObj[S] {

    def tpe: Obj.Type = SpanLikeObj

    private[lucre] def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
      new Tuple2[Out, T1, ReprT1, T2, ReprT2](Targets[Out], op, context(_1), context(_2)).connect()
  }

  // ---- operators ----

  final class Ops[S <: Sys[S]](val `this`: _Ex[S]) extends AnyVal { me =>
    import me.{`this` => ex}
    // ---- binary ----
    def shift(delta: LongObj[S])(implicit tx: S#Tx): _Ex[S] = BinaryOp.Shift(ex, delta)
  }

  private object UnaryOp {
    sealed trait Op[T1, ReprT1[~ <: Sys[~]] <: Expr[~, T1]] {
      def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                           (implicit tx: S#Tx): _Ex[S]

      def toString[S <: Sys[S]](_1: ReprT1[S]): String = s"$name(${_1})"

      def name: String = {
        val cn  = getClass.getName
        val sz  = cn.length
        val i   = cn.lastIndexOf('$', sz - 2) + 1
        s"${cn.charAt(i).toLower}${cn.substring(i + 1, if (cn.charAt(sz - 1) == '$') sz - 1 else sz)}"
      }
    }

    sealed abstract class LongOp extends impl.Tuple1Op[SpanLike, Long, SpanLikeObj, LongObj]
      with Op[Long, LongObj] {

      final def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                 (implicit tx: S#Tx): _Ex[S] = {
        val _1 = LongObj.read(in, access)
        new Tuple1[S, Long, LongObj](targets, this, _1)
      }

      final def apply[S <: Sys[S]](a: LongObj[S])(implicit tx: S#Tx): _Ex[S] =
        new Tuple1[S, Long, LongObj](Targets[S], this, a).connect()
    }

    case object From extends LongOp {
      final val id = 0
      def value(a: Long): SpanLike = Span.from(a)

      override def toString[S <: Sys[S]](_1: LongObj[S]): String = s"Span.from(${_1 })"
    }

    case object Until extends LongOp {
      final val id = 1
      def value(a: Long): SpanLike = Span.until(a)

      override def toString[S <: Sys[S]](_1: LongObj[S]): String = s"Span.until(${_1})"
    }
  }

  private object BinaryOp {
    sealed trait Op[T1, T2, ReprT1[~ <: Sys[~]] <: Expr[~, T1], ReprT2[~ <: Sys[~]] <: Expr[~, T2]] {
      def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                           (implicit tx: S#Tx): _Ex[S]

      def toString[S <: Sys[S]](_1: ReprT1[S], _2: ReprT2[S]): String =
        s"${_1}.$name(${_2})"

      def value(a: T1, b: T2): SpanLike

      def name: String = {
        val cn = getClass.getName
        val sz = cn.length
        val i = cn.lastIndexOf('$', sz - 2) + 1
        s"${cn.charAt(i).toLower}${cn.substring(i + 1, if (cn.charAt(sz - 1) == '$') sz - 1 else sz)}"
      }
    }

    sealed abstract class LongSpanOp
      extends impl.Tuple2Op[SpanLike, SpanLike, Long, SpanLikeObj, SpanLikeObj, LongObj]
      with Op[SpanLike, Long, SpanLikeObj, LongObj] {

      final def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                 (implicit tx: S#Tx): _Ex[S] = {
        val _1 = SpanLikeObj.read(in, access)
        val _2 = LongObj    .read(in, access)
        new Tuple2[S, SpanLike, SpanLikeObj, Long, LongObj](targets, this, _1, _2)
      }

      final def apply[S <: Sys[S]](a: _Ex[S], b: LongObj[S])(implicit tx: S#Tx): _Ex[S] = (a, b) match {
        case (Expr.Const(ca), Expr.Const(cb)) => SpanLikeObj.newConst(value(ca, cb))
        case _  =>
          new Tuple2[S, SpanLike, SpanLikeObj, Long, LongObj](Targets[S], this, a, b).connect()
      }
    }

    sealed abstract class LongLongOp
      extends impl.Tuple2Op[SpanLike, Long, Long, SpanLikeObj, LongObj, LongObj]
      with Op[Long, Long, LongObj, LongObj] {

      final def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                 (implicit tx: S#Tx): _Ex[S] = {
        val _1 = LongObj.read(in, access)
        val _2 = LongObj.read(in, access)
        new Tuple2[S, Long, LongObj, Long, LongObj](targets, this, _1, _2)
      }

      final def apply[S <: Sys[S]](a: LongObj[S], b: LongObj[S])(implicit tx: S#Tx): _Ex[S] =
        new Tuple2[S, Long, LongObj, Long, LongObj](Targets[S], this, a, b).connect()
    }

    case object Apply extends LongLongOp {
      final val id = 2
      override def toString[S <: Sys[S]](_1: LongObj[S], _2: LongObj[S]): String =
        s"Span(${_1}, ${_2})"

      def value(a: Long, b: Long): SpanLike = Span(a, b)
    }

    case object Shift extends LongSpanOp {
      final val id = 3
      def value(a: SpanLike, b: Long): SpanLike = a.shift(b)
    }
  }
}