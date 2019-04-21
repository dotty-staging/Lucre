/*
 *  IntExtensions.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2019 Hanns Holger Rutz. All rights reserved.
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
import de.sciss.lucre.stm.{Copy, Elem, Obj, Sys}
import de.sciss.serial.DataInput

import scala.annotation.switch
import scala.language.higherKinds

object IntExtensions {
  private[this] lazy val _init: Unit = {
    IntObj.registerExtension(IntTuple1s)
    IntObj.registerExtension(IntTuple2s)
  }

  def init(): Unit = _init

  type _Ex[S <: Sys[S]] = IntObj[S]

  private[this] object IntTuple1s extends Type.Extension1[IntObj] {
    // final val arity = 1
    final val opLo: Int = Neg         .id
    final val opHi: Int = BooleanToInt.id

    val name = "Int-1 Ops"

    def readExtension[S <: Sys[S]](opId: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                                  (implicit tx: S#Tx): _Ex[S] = {
      val op /* : UnaryOp[_, _] */ = (opId: @switch) match {
        // ---- Int => Int ----
        case Neg    .id => Neg
        case BitNot .id => BitNot
        case Abs    .id => Abs
        case Signum .id => Signum
        case Squared.id => Squared
        case Cubed  .id => Cubed
        // ---- Boolean => Int ----
        case BooleanToInt.id => BooleanToInt
      }
      op.read(in, access, targets)
    }
  }

  private[this] object IntTuple2s extends Type.Extension1[IntObj] {
    // final val arity = 2
    final val opLo: Int = Plus  .id
    final val opHi: Int = Absdif.id

    val name = "Int-2 Ops"

    def readExtension[S <: Sys[S]](opId: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                                  (implicit tx: S#Tx): _Ex[S] = {
      val op: BinaryOp = (opId: @switch) match {
        case Plus               .id => Plus
        case Minus              .id => Minus
        case Times              .id => Times
        case IDiv               .id => IDiv
        //               case 4 => Div
        //               case 5 => Mod
        //      case 6 => Eq
        //      case 7 => Neq
        //      case 8 => Lt
        //      case 9 => Gt
        //      case 10 => Leq
        //      case 11 => Geq
        case Min                .id => Min
        case Max                .id => Max
        case BitAnd             .id => BitAnd
        case BitOr              .id => BitOr
        case BitXor             .id => BitXor
        // case 17 => Lcm
        // case 18 => Gcd
        //               case 19 => Round
        //               case 20 => Roundup
        case ShiftLeft          .id => ShiftLeft
        case ShiftRight         .id => ShiftRight
        case UnsignedShiftRight .id => UnsignedShiftRight
        case Absdif             .id => Absdif
        //               case 42 => Clip2
        //               case 44 => Fold2
        //               case 45 => Wrap2
      }
      val _1 = IntObj.read(in, access)
      val _2 = IntObj.read(in, access)
      new Tuple2[S, Int, IntObj, Int, IntObj](targets, op, _1, _2)
    }
  }

  final class Tuple2[S <: Sys[S], T1, ReprT1[~ <: Sys[~]] <: Expr[~, T1],
                                  T2, ReprT2[~ <: Sys[~]] <: Expr[~, T2]](
      protected val targets: Targets[S], val op: Tuple2Op[Int, T1, T2, IntObj, ReprT1, ReprT2],
      val _1: ReprT1[S], val _2: ReprT2[S])
    extends impl.Tuple2[S, Int, T1, T2, IntObj, ReprT1, ReprT2] with IntObj[S] {

    def tpe: Obj.Type = IntObj

    private[lucre] def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
      new Tuple2[Out, T1, ReprT1, T2, ReprT2](Targets[Out], op, context(_1), context(_2)).connect()
  }

  // ---- operators ----

  sealed trait UnaryOp[T1, ReprT1[~ <: Sys[~]] <: Expr[~, T1]] extends impl.Tuple1Op[Int, T1, IntObj, ReprT1] {
    def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: Targets[S])
                         (implicit tx: S#Tx): _Ex[S] //  impl.Tuple1[S, Int, T1]

    def toString[S <: Sys[S]](_1: ReprT1[S]): String = s"${_1}.$name"

    def apply[S <: Sys[S]](a: ReprT1[S])(implicit tx: S#Tx): _Ex[S] = a match {
      case Expr.Const(c)  => IntObj.newConst[S](value(c))
      case _              => new Tuple1[S, T1, ReprT1](Targets[S], this, a).connect()
    }

    def name: String = {
      val cn  = getClass.getName
      val sz  = cn.length
      val i   = cn.lastIndexOf('$', sz - 2) + 1
      s"${cn.charAt(i).toLower}${cn.substring(i + 1, if (cn.charAt(sz - 1) == '$') sz - 1 else sz)}"
    }
  }

  final class Tuple1[S <: Sys[S], T1, ReprT1[~ <: Sys[~]] <: Expr[~, T1]](
      protected val targets: Targets[S], val op: Tuple1Op[Int, T1, IntObj, ReprT1], val _1: ReprT1[S])
    extends impl.Tuple1[S, Int, T1, IntObj, ReprT1] with IntObj[S] {

    def tpe: Obj.Type = IntObj

    private[lucre] def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
      new Tuple1[Out, T1, ReprT1](Targets[Out], op, context(_1)).connect()
  }

  // ---- Int => Int ----

  private[this] sealed abstract class IntUnaryOp extends UnaryOp[Int, IntObj] {
    final def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: Targets[S])
                               (implicit tx: S#Tx): _Ex[S] = {
      val _1 = IntObj.read(in, access)
      new Tuple1[S, Int, IntObj](targets, this, _1)
    }
  }

  private[this] case object Neg extends IntUnaryOp {
    final val id = 0
    def value(a: Int): Int = -a
    override def toString[S <: Sys[S]](_1: _Ex[S]): String = s"-${_1}"
  }

  private[this] case object Abs extends IntUnaryOp {
    final val id = 1
    def value(a: Int): Int = math.abs(a)
  }

  private[this] case object BitNot extends IntUnaryOp {
    final val id = 2
    def value(a: Int): Int = ~a
    override def toString[S <: Sys[S]](_1: _Ex[S]): String = s"~${_1}"
  }

  // case object ToLong     extends Op(  6 )
  // case object ToInt       extends Op(  7 )
  private[this] case object Signum extends IntUnaryOp {
    final val id = 3
    def value(a: Int): Int = math.signum(a)
  }

  private[this] case object Squared extends IntUnaryOp {
    final val id = 4
    def value(a: Int): Int = a * a
  }

  private[this] case object Cubed extends IntUnaryOp {
    final val id = 5
    def value(a: Int): Int = a * a * a
  }

  // ---- Boolean => Int ----

  sealed trait BooleanUnaryOp extends UnaryOp[Boolean, BooleanObj] {
    final def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: Targets[S])
                               (implicit tx: S#Tx): _Ex[S] = {
      val _1 = BooleanObj.read(in, access)
      new Tuple1[S, Boolean, BooleanObj](targets, this, _1)
    }
  }

  case object BooleanToInt extends BooleanUnaryOp {
    final val id = 6
    def value(a: Boolean): Int = if (a) 1 else 0
  }

  // ---- (Int, Int) => Int ----

  private[this] sealed trait BinaryOp extends impl.Tuple2Op[Int, Int, Int, IntObj, IntObj, IntObj] {
    final def apply[S <: Sys[S]](a: _Ex[S], b: _Ex[S])(implicit tx: S#Tx): _Ex[S] = (a, b) match {
      case (Expr.Const(ca), Expr.Const(cb)) => IntObj.newConst(value(ca, cb))
      case _ =>
        new Tuple2[S, Int, IntObj, Int, IntObj](Targets[S], this,  a, b).connect()
    }

    def value(a: Int, b: Int): Int

    def isInfix: Boolean

    final def toString[S <: Sys[S]](_1: _Ex[S], _2: _Ex[S]): String =
      if (isInfix) s"(${_1} $name ${_2})" else s"${_1}.$name(${_2})"

    def name: String = {
      val cn = getClass.getName
      val sz = cn.length
      val i  = cn.indexOf('$') + 1
      s"${cn.charAt(i).toLower}${cn.substring(i + 1, if (cn.charAt(sz - 1) == '$') sz - 1 else sz)}"
    }
  }

  private[this] case object Plus extends BinaryOp {
    final val id = 7
    override val name = "+"
    def value(a: Int, b: Int): Int = a + b
    final val isInfix = true
  }

  private[this] case object Minus extends BinaryOp {
    final val id = 8
    override val name = "-"
    def value(a: Int, b: Int): Int = a - b
    final val isInfix = true
  }

  private[this] case object Times extends BinaryOp {
    final val id = 9
    override val name = "*"
    def value(a: Int, b: Int): Int = a * b
    final val isInfix = true
  }

  private[this] case object IDiv extends BinaryOp {
    final val id = 10
    override val name = "div"
    def value(a: Int, b: Int): Int = a / b
    val isInfix = false
  }

  private[this] case object Min extends BinaryOp {
    final val id = 11
    def value(a: Int, b: Int): Int = math.min(a, b)
    val isInfix = false
  }

  private[this] case object Max extends BinaryOp {
    final val id = 12
    def value(a: Int, b: Int): Int = math.max(a, b)
    val isInfix = false
  }

  private[this] case object BitAnd extends BinaryOp {
    final val id = 13
    def value(a: Int, b: Int): Int = a & b
    val isInfix = false
  }

  private[this] case object BitOr extends BinaryOp {
    final val id = 14
    def value(a: Int, b: Int): Int = a | b
    val isInfix = false
  }

  private[this] case object BitXor extends BinaryOp {
    final val id = 15
    def value(a: Int, b: Int): Int = a ^ b
    val isInfix = false
  }

  private[this] case object ShiftLeft extends BinaryOp {
    final val id = 16
    override val name = "<<"
    def value(a: Int, b: Int): Int = a << b
    val isInfix = false
  }

  private[this] case object ShiftRight extends BinaryOp {
    final val id = 17
    override val name = ">>"
    def value(a: Int, b: Int): Int = a >> b
    val isInfix = false
  }

  private[this] case object UnsignedShiftRight extends BinaryOp {
    final val id = 18
    override val name = ">>>"
    def value(a: Int, b: Int): Int = a >>> b
    val isInfix = false
  }

  private[this] case object Absdif extends BinaryOp {
    final val id = 19
    def value(a: Int, b: Int): Int = math.abs(a - b)
    val isInfix = false
  }

  //      case object Clip2          extends Op( 42 ) {
  //         def value( a: Int, b: Int ) : Int = ri_clip2( a, b )
  //      }
  //      case object Fold2          extends Op( 44 ) {
  //         def value( a: Int, b: Int ) : Int = ri_fold2( a, b )
  //      }
  //      case object Wrap2          extends Op( 45 ) {
  //         def value( a: Int, b: Int ) : Int = ri_wrap2( a, b )
  //      }

  final class Ops[S <: Sys[S]](val `this`: _Ex[S]) extends AnyVal { me =>
    import me.{`this` => a}

    private type E = _Ex[S]

    // ---- Int => Int ----

    def unary_- (implicit tx: S#Tx): E = Neg   (a)
    def unary_~ (implicit tx: S#Tx): E = BitNot(a)

    // ---- (Int, Int) => Int ----

    def +   (b: E)(implicit tx: S#Tx): E = Plus              (a, b)
    def -   (b: E)(implicit tx: S#Tx): E = Minus             (a, b)
    def *   (b: E)(implicit tx: S#Tx): E = Times             (a, b)
    def /   (b: E)(implicit tx: S#Tx): E = IDiv              (a, b)
    def &   (b: E)(implicit tx: S#Tx): E = BitAnd            (a, b)
    def |   (b: E)(implicit tx: S#Tx): E = BitOr             (a, b)
    def ^   (b: E)(implicit tx: S#Tx): E = BitXor            (a, b)
    def <<  (b: E)(implicit tx: S#Tx): E = ShiftLeft         (a, b)
    def >>  (b: E)(implicit tx: S#Tx): E = ShiftRight        (a, b)
    def >>> (b: E)(implicit tx: S#Tx): E = UnsignedShiftRight(a, b)

    // ---- (Int, Int) => Boolean ----

    def sig_==(b: E)(implicit tx: S#Tx): BooleanObj[S] = BooleanExtensions.IntEq (a, b)
    def sig_!=(b: E)(implicit tx: S#Tx): BooleanObj[S] = BooleanExtensions.IntNeq(a, b)
    def <     (b: E)(implicit tx: S#Tx): BooleanObj[S] = BooleanExtensions.IntLt (a, b)
    def >     (b: E)(implicit tx: S#Tx): BooleanObj[S] = BooleanExtensions.IntGt (a, b)
    def <=    (b: E)(implicit tx: S#Tx): BooleanObj[S] = BooleanExtensions.IntLeq(a, b)
    def >=    (b: E)(implicit tx: S#Tx): BooleanObj[S] = BooleanExtensions.IntGeq(a, b)

    // ---- more ops ----

    def abs     (implicit tx: S#Tx): E = Abs     (a)
    // def toLong : E	         = UnOp.make( 'asLong, ex )
    // def toInteger : E	      = UnOp.make( 'asInteger, ex )
    def signum  (implicit tx: S#Tx): E = Signum  (a)
    def squared (implicit tx: S#Tx): E = Squared (a)
    def cubed   (implicit tx: S#Tx): E = Cubed   (a)

    def min   (b: E)(implicit tx: S#Tx): E = Min   (a, b)
    def max   (b: E)(implicit tx: S#Tx): E = Max   (a, b)
    def absdif(b: E)(implicit tx: S#Tx): E = Absdif(a, b)

    //      def clip2( b: E ) : E      = Clip2.make( ex, b )
    //      def fold2( b: E ) : E      = Fold2.make( ex, b )
    //      def wrap2( b: E ) : E      = Wrap2.make( ex, b )
  }
}