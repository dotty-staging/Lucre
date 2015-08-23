/*
 *  LongExtensions.scala
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

import de.sciss.lucre
import de.sciss.lucre.event.Targets
import de.sciss.lucre.stm.Sys
import de.sciss.serial.DataInput

import scala.annotation.switch

object LongExtensions {
  private[this] lazy val _init: Unit = {
    LongObj.registerExtension(LongTuple1s)
    LongObj.registerExtension(LongTuple2s)
  }

  def init(): Unit = _init

  type Ex[S <: Sys[S]] = LongObj[S]

  private[this] object LongTuple1s extends Type.Extension1[LongObj] {
    // final val arity = 1
    final val opLo  = UnaryOp.Neg  .id
    final val opHi  = UnaryOp.Cubed.id

    val name = "Long-Long Ops"

    def readExtension[S <: Sys[S]](opID: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                                  (implicit tx: S#Tx): Expr[S, Long] = {
      import UnaryOp._
      val op: Op[_] = (opID: @switch) match {
        // ---- Long ----
        case Neg    .id => Neg
        case BitNot .id => BitNot
        case Abs    .id => Abs
        case Signum .id => Signum
        case Squared.id => Squared
        case Cubed  .id => Cubed
      }
      op.read(in, access, targets)
    }
  }

  private[this] object LongTuple2s extends Type.Extension1[LongObj] {
    // final val arity = 2
    final val opLo  = BinaryOp.Plus  .id
    final val opHi  = BinaryOp.Absdif.id

    val name = "Long-Long Ops"

    def readExtension[S <: Sys[S]](opID: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                                  (implicit tx: S#Tx): Ex[S] = {
      import BinaryOp._
      val op: Op = (opID: @switch) match {
        // ---- Long ----
        case Plus   .id => Plus
        case Minus  .id => Minus
        case Times  .id => Times
        case IDiv   .id => IDiv
        //               case 4 => Div
        //               case 5 => Mod
        //      case 6 => Eq
        //      case 7 => Neq
        //      case 8 => Lt
        //      case 9 => Gt
        //      case 10 => Leq
        //      case 11 => Geq
        case Min    .id => Min
        case Max    .id => Max
        case BitAnd .id => BitAnd
        case BitOr  .id => BitOr
        case BitXor .id => BitXor
        // case 17 => Lcm
        // case 18 => Gcd
        //               case 19 => Round
        //               case 20 => Roundup
        //               case 26 => <<
        //               case 27 => >>
        //               case 28 => >>>
        case Absdif .id => Absdif
        //               case 42 => Clip2
        //               case 44 => Fold2
        //               case 45 => Wrap2
      }
      val _1 = LongObj.read(in, access)
      val _2 = LongObj.read(in, access)
      ??? // RRR new impl.Tuple2(LongObj, op, targets, _1, _2)
    }
  }

  // ---- operators ----

  object UnaryOp {
    trait Op[T1] extends impl.Tuple1Op[Long, T1] {
      def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: Targets[S])
                           (implicit tx: S#Tx): impl.Tuple1[S, Long, T1]

      def toString[S <: Sys[S]](_1: Expr[S, T1]): String = s"${_1}.$name"

      def apply[S <: Sys[S]](a: Expr[S, T1])(implicit tx: S#Tx): Ex[S] = a match {
        case Expr.Const(c)  => LongObj.newConst(value(c))
        case _              => ??? // RRR new impl.Tuple1(LongObj, this, Targets[S], a).connect()
      }

      def name: String = {
        val cn = getClass.getName
        val sz = cn.length
        val i  = cn.lastIndexOf('$', sz - 2) + 1
        s"${cn.charAt(i).toLower}${cn.substring(i + 1, if (cn.charAt(sz - 1) == '$') sz - 1 else sz)}"
      }
    }

    sealed abstract class LongOp extends Op[Long] {
      // def id: Int

      final def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: Targets[S])
                                 (implicit tx: S#Tx): impl.Tuple1[S, Long, Long] = {
        val _1 = LongObj.read(in, access)
        new impl.Tuple1(LongObj, this, targets, _1)
      }
    }

    case object Neg extends LongOp {
      final val id = 0
      def value(a: Long): Long = -a
      override def toString[S <: Sys[S]](_1: Ex[S]): String = s"-${_1}"
    }

    case object Abs extends LongOp {
      final val id = 1
      def value(a: Long): Long = math.abs(a)
    }

    case object BitNot extends LongOp {
      final val id = 2
      def value(a: Long): Long = ~a
      override def toString[S <: Sys[S]](_1: Ex[S]): String = s"~${_1}"
    }

    // case object ToLong     extends Op(  6 )
    // case object ToInt       extends Op(  7 )

    case object Signum extends LongOp {
      final val id = 3
      def value(a: Long): Long = math.signum(a)
    }

    case object Squared extends LongOp {
      final val id = 4
      def value(a: Long): Long = a * a
    }

    case object Cubed extends LongOp {
      final val id = 5
      def value(a: Long): Long = a * a * a
    }
  }

  private object BinaryOp {
    sealed trait Op extends impl.Tuple2Op[Long, Long, Long] {
      final def apply[S <: Sys[S]](a: Ex[S], b: Ex[S])(implicit tx: S#Tx): Ex[S] = (a, b) match {
        case (Expr.Const(ca), Expr.Const(cb)) => LongObj.newConst(value(ca, cb))
        case _                                => ??? // RRR new impl.Tuple2(LongObj, this, Targets[S], a, b).connect()
      }

      def value(a: Long, b: Long): Long

      def toString[S <: Sys[S]](_1: Ex[S], _2: Ex[S]): String = s"${_1}.$name(${_2})"

      def name: String = {
        val cn = getClass.getName
        val sz = cn.length
        val i  = cn.indexOf('$') + 1
        "" + cn.charAt(i).toLower + cn.substring(i + 1, if (cn.charAt(sz - 1) == '$') sz - 1 else sz)
      }
    }

    trait Infix {
      _: Op =>

      override def toString[S <: Sys[S]](_1: Ex[S], _2: Ex[S]): String = s"(${_1} $name ${_2})"
    }

    case object Plus extends Op with Infix {
      final val id = 6
      override val name = "+"
      def value(a: Long, b: Long): Long = a + b
    }

    case object Minus extends Op with Infix {
      final val id = 7
      override val name = "-"
      def value(a: Long, b: Long): Long = a - b
    }

    case object Times extends Op with Infix {
      final val id = 8
      override val name = "*"
      def value(a: Long, b: Long): Long = a * b
    }

    case object IDiv extends Op {
      final val id = 9
      override val name = "div"
      def value(a: Long, b: Long): Long = a / b
    }

    case object Min extends Op {
      final val id = 10
      def value(a: Long, b: Long): Long = math.min(a, b)
    }

    case object Max extends Op {
      final val id = 11
      def value(a: Long, b: Long): Long = math.max(a, b)
    }

    case object BitAnd extends Op {
      final val id = 12
      def value(a: Long, b: Long): Long = a & b
    }

    case object BitOr extends Op {
      final val id = 13
      def value(a: Long, b: Long): Long = a | b
    }

    case object BitXor extends Op {
      final val id = 14
      def value(a: Long, b: Long): Long = a ^ b
    }

    //      case object <<             extends Op( 26 ) {
    //         def value( a: Long, b: Long ) : Long = a << b
    //      }
    //      case object >>             extends Op( 27 ) {
    //         def value( a: Long, b: Long ) : Long = a >> b
    //      }
    //      case object >>>            extends Op( 28 ) {
    //         def value( a: Long, b: Long ) : Long = a >>> b
    //      }
    case object Absdif extends Op {
      final val id = 15
      def value(a: Long, b: Long): Long = math.abs(a - b)
    }

    //      case object Clip2          extends Op( 42 ) {
    //         def value( a: Long, b: Long ) : Long = rd_clip2( a, b )
    //      }
    //      case object Fold2          extends Op( 44 ) {
    //         def value( a: Long, b: Long ) : Long = rd_fold2( a, b )
    //      }
    //      case object Wrap2          extends Op( 45 ) {
    //         def value( a: Long, b: Long ) : Long = rd_wrap2( a, b )
    //      }
  }

  final class Ops[S <: Sys[S]](val `this`: Ex[S]) extends AnyVal { me =>
    import me.{`this` => a}
    private type E = Ex[S]

    import UnaryOp._

    def unary_- (implicit tx: S#Tx): E = Neg   (a)
    def unary_~ (implicit tx: S#Tx): E = BitNot(a)

    import BinaryOp._

    def + (b: E)(implicit tx: S#Tx): E = Plus  (a, b)
    def - (b: E)(implicit tx: S#Tx): E = Minus (a, b)
    def * (b: E)(implicit tx: S#Tx): E = Times (a, b)
    def / (b: E)(implicit tx: S#Tx): E = IDiv  (a, b)
    def & (b: E)(implicit tx: S#Tx): E = BitAnd(a, b)
    def | (b: E)(implicit tx: S#Tx): E = BitOr (a, b)
    def ^ (b: E)(implicit tx: S#Tx): E = BitXor(a, b)

    def abs     (implicit tx: S#Tx): E = Abs     (a)

    // def toLong : E	         = UnOp( 'asLong, ex )
    // def toInteger : E	      = UnOp( 'asInteger, ex )
    def signum  (implicit tx: S#Tx): E = Signum  (a)
    def squared (implicit tx: S#Tx): E = Squared (a)
    def cubed   (implicit tx: S#Tx): E = Cubed   (a)

    import BinaryOp._

    def min   (b: E)(implicit tx: S#Tx): E = Min   (a, b)
    def max   (b: E)(implicit tx: S#Tx): E = Max   (a, b)
    def absdif(b: E)(implicit tx: S#Tx): E = Absdif(a, b)

    //      def clip2( b: E ) : E      = Clip2( ex, b )
    //      def fold2( b: E ) : E      = Fold2( ex, b )
    //      def wrap2( b: E ) : E      = Wrap2( ex, b )
  }
}