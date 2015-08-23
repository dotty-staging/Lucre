/*
 *  SpanExtensions.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2015 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr

import de.sciss.lucre.event.Targets
import de.sciss.lucre.expr.impl.{Tuple1Op, Tuple2Op}
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.serial.DataInput
import de.sciss.span.Span

import scala.annotation.switch
import scala.language.higherKinds

object SpanExtensions  {
  private[this] lazy val _init: Unit = {
    LongObj.registerExtension(LongTuple1s)
    SpanObj.registerExtension(SpanTuple2s)
  }

  def init(): Unit = _init

  type Ex[S <: Sys[S]] = SpanObj[S]

  private[this] object LongTuple1s extends Type.Extension1[LongObj] {
    // final val arity = 1
    final val opLo  = UnaryOp.Start .id
    final val opHi  = UnaryOp.Length.id

    val name = "Span-Long Ops"

    def readExtension[S <: Sys[S]](opID: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                                  (implicit tx: S#Tx): LongObj[S] = {
      import UnaryOp._
      val op: LongOp = (opID: @switch) match {
        // ---- Span ----
        case Start  .id => Start
        case Stop   .id => Stop
        case Length .id => Length
      }
      op.read(in, access, targets)
    }
  }

  private[this] object SpanTuple2s extends Type.Extension1[SpanObj] {
    // final val arity = 2
    final val opLo  = BinaryOp.Apply.id
    final val opHi  = BinaryOp.Shift.id

    val name = "Int-Int Ops"

    def readExtension[S <: Sys[S]](opID: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                                  (implicit tx: S#Tx): Ex[S] = {
      import BinaryOp._
      val op /* : Op[_, _, _, _] */ = opID /* : @switch */ match {
        case Apply.id => Apply
        case Shift.id => Shift
      }
      op.read(in, access, targets)
    }
  }

  final class Ops2(val `this`: Span.type) extends AnyVal { me =>
    // import me.{`this` => ex}
    def apply[S <: Sys[S]](start: LongObj[S], stop: LongObj[S])(implicit tx: S#Tx): Ex[S] =
      (start, stop) match {
        case (Expr.Const(startC), Expr.Const(stopC)) => SpanObj.newConst(Span(startC, stopC))
        case _ =>
          new Tuple2(Targets[S], BinaryOp.Apply, start, stop).connect()
      }
  }

  final class Tuple2[S <: Sys[S], T1, ReprT1[~ <: Sys[~]] <: Expr[~, T1],
                                  T2, ReprT2[~ <: Sys[~]] <: Expr[~, T2]](
      protected val targets: Targets[S], val op: Tuple2Op[Span, T1, T2, SpanObj, ReprT1, ReprT2],
      val _1: ReprT1[S], val _2: ReprT2[S])
    extends impl.Tuple2[S, Span, T1, T2, SpanObj, ReprT1, ReprT2] with SpanObj[S] {

    def tpe: Obj.Type = SpanObj
  }

  // XXX TODO: fold constants
  final class Ops[S <: Sys[S]](val `this`: Ex[S]) extends AnyVal { me =>
    import me.{`this` => ex}
    // ---- unary ----
    def start (implicit tx: S#Tx): LongObj[S] = UnaryOp.Start (ex)
    def stop  (implicit tx: S#Tx): LongObj[S] = UnaryOp.Stop  (ex)
    def length(implicit tx: S#Tx): LongObj[S] = UnaryOp.Length(ex)

    // ---- binary ----
    def shift(delta: LongObj[S])(implicit tx: S#Tx): Ex[S] = BinaryOp.Shift(ex, delta)
  }

  // ----- operators -----

  object UnaryOp {
    //      sealed trait OpLike[ T1 ] {
    //         def toString[ S <: Sys[ S ]]( _1: Expr[ S, T1 ]) : String = _1.toString + "." + name
    //
    //         def name: String = { val cn = getClass.getName
    //            val sz   = cn.length
    //            val i    = cn.indexOf( '$' ) + 1
    //            "" + cn.charAt( i ).toLower + cn.substring( i + 1, if( cn.charAt( sz - 1 ) == '$' ) sz - 1 else sz )
    //         }
    //      }

    sealed abstract class LongOp extends LongExtensions.UnaryOp.Op[Span, SpanObj] /* Longs.UnaryOp.Op[Span] */ {
      final def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: Targets[S])
                                 (implicit tx: S#Tx): LongObj[S] = {
        val _1 = SpanObj.read(in, access)
        new LongExtensions.Tuple1(targets, this, _1)
      }
    }

//    private[this] final class Tuple1[S <: Sys[S], T1, ReprT1[~ <: Sys[~]] <: Expr[~, T1]](
//        protected val targets: Targets[S], val op: Tuple1Op[Span, T1, SpanObj, ReprT1], val _1: ReprT1[S])
//      extends impl.Tuple1[S, Span, T1, SpanObj, ReprT1] with SpanObj[S] {
//
//      def tpe: Obj.Type = SpanObj
//    }

    case object Start extends LongOp {
      final val id = 20
      def value(a: Span): Long = a.start
    }

    case object Stop extends LongOp {
      final val id = 21
      def value(a: Span): Long = a.stop
    }

    case object Length extends LongOp {
      final val id = 22
      def value(a: Span): Long = a.length
    }
  }

  private object BinaryOp {
    sealed trait Op[T1, T2, ReprT1[~ <: Sys[~]] <: Expr[~, T1], ReprT2[~ <: Sys[~]] <: Expr[~, T2]] {
      def toString[S <: Sys[S]](_1: ReprT1[S], _2: ReprT2[S]): String = s"${_1}.$name(${_2})"

      def name: String = {
        val cn = getClass.getName
        val sz = cn.length
        val i  = cn.lastIndexOf('$', sz - 2) + 1
        s"${cn.charAt(i).toLower}${cn.substring(i + 1, if (cn.charAt(sz - 1) == '$') sz - 1 else sz)}"
      }

      def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: Targets[S])
                           (implicit tx: S#Tx): SpanObj[S] // impl.Tuple2[S, Span, T1, T2]
    }

    sealed abstract class LongSpanOp(val id: Int)
      extends impl.Tuple2Op[Span, Span, Long, SpanObj, SpanObj, LongObj]
      with Op[Span, Long, SpanObj, LongObj] {

      final def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: Targets[S])
                                 (implicit tx: S#Tx): Ex[S] = {
        val _1 = SpanObj.read(in, access)
        val _2 = LongObj.read(in, access)
        new Tuple2(targets, this, _1, _2)
      }

      final def apply[S <: Sys[S]](a: Ex[S], b: LongObj[S])(implicit tx: S#Tx): Ex[S] = (a, b) match {
        case (Expr.Const(ca), Expr.Const(cb)) => SpanObj.newConst(value(ca, cb))
        case _ => 
          new Tuple2(Targets[S], this, a, b).connect()
      }
    }

    object Apply extends impl.Tuple2Op[Span, Long, Long, SpanObj, LongObj, LongObj]
      with Op[Long, Long, LongObj, LongObj] {

      final val id = 0

      def value(a: Long, b: Long): Span = Span(a, b)

      override def toString[S <: Sys[S]](_1: LongObj[S], _2: LongObj[S]): String = s"Span(${_1}, ${_2})"

      def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: Targets[S])
                           (implicit tx: S#Tx): Ex[S] = {
        val _1 = LongObj.read(in, access)
        val _2 = LongObj.read(in, access)
        new Tuple2(targets, this, _1, _2)
      }
    }

    case object Shift extends LongSpanOp(1) {
      def value(a: Span, b: Long): Span = a.shift(b)
    }
  }
}