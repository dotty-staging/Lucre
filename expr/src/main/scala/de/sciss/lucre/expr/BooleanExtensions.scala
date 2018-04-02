/*
 *  BooleanExtensions.scala
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

import de.sciss.lucre.event.Targets
import de.sciss.lucre.expr.impl.{Tuple2Op, Tuple1Op}
import de.sciss.lucre.stm.{Elem, Copy, Obj, Sys}
import de.sciss.lucre.{event => evt}
import de.sciss.model.Change
import de.sciss.serial.{DataInput, DataOutput}

import scala.annotation.switch
import scala.language.higherKinds

object BooleanExtensions  {
  private[this] lazy val _init: Unit = {
    BooleanObj.registerExtension(BooleanTuple1s)
    BooleanObj.registerExtension(BooleanTuple2s)
  }

  def init(): Unit = _init

  private[this] type Ex[S <: Sys[S]] = BooleanObj[S]

  private[this] object BooleanTuple1s extends Type.Extension1[BooleanObj] {
    // final val arity = 1
    final val opLo: Int = Not.id
    final val opHi: Int = Not.id

    val name = "Boolean-1 Ops"

    def readExtension[S <: Sys[S]](opID: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                                  (implicit tx: S#Tx): Ex[S] = {
      val op: UnaryOp[Boolean, BooleanObj] = opID /* : @switch */ match {
        case Not.id => Not
      }
      val _1 = BooleanObj.read(in, access)
      new Tuple1[S, Boolean, BooleanObj](targets, op, _1)
    }
  }

  final class Tuple1[S <: Sys[S], T1, ReprT1[~ <: Sys[~]] <: Expr[~, T1]](
    protected val targets: Targets[S], val op: Tuple1Op[Boolean, T1, BooleanObj, ReprT1], val _1: ReprT1[S])
    extends impl.Tuple1[S, Boolean, T1, BooleanObj, ReprT1] with BooleanObj[S] {

    def tpe: Obj.Type = BooleanObj

    private[lucre] def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
      new Tuple1[Out, T1, ReprT1](Targets[Out], op, context(_1)).connect()
  }

  private[this] object BooleanTuple2s extends Type.Extension1[BooleanObj] {
    // final val arity = 2
    final val opLo: Int = And   .id
    final val opHi: Int = IntGeq.id

    val name = "Boolean-2 Ops"

    def readExtension[S <: Sys[S]](opID: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                                  (implicit tx: S#Tx): Ex[S] = {
      val op /* : BinaryOp[_, _] */ = (opID: @switch) match {
        case And   .id => And
        case Or    .id => Or
        case Xor   .id => Xor

        case IntEq .id => IntEq
        case IntNeq.id => IntNeq
        case IntLt .id => IntLt
        case IntGt .id => IntGt
        case IntLeq.id => IntLeq
        case IntGeq.id => IntGeq
      }
      op.read(in, access, targets)
    }
  }

  // ----- operators -----

  final class Ops[S <: Sys[S]](val `this`: Ex[S]) extends AnyVal { me =>
    import me.{`this` => a}
    private type E = Ex[S]

    // ---- Boolean => Boolean ----

    def unary_!(implicit tx: S#Tx): E = Not(a)

    // ---- (Boolean, Boolean) => Boolean ----

    def && (b: E)(implicit tx: S#Tx): E = And(a, b)
    def || (b: E)(implicit tx: S#Tx): E = Or (a, b)
    def ^  (b: E)(implicit tx: S#Tx): E = Xor(a, b)

    // ---- Boolean => Int ----

    def toInt(implicit tx: S#Tx): IntObj[S] = IntExtensions.BooleanToInt(a)
  }

  // ----- impl -----

  private[this] abstract class UnaryOp[T1, ReprT1[~ <: Sys[~]] <: Expr[~, T1]]
    extends impl.Tuple1Op[Boolean, T1, BooleanObj, ReprT1] {

    final def apply[S <: Sys[S]](_1: ReprT1[S])(implicit tx: S#Tx): Ex[S] = _1 match {
      case Expr.Const(a) => BooleanObj.newConst[S](value(a))
      case _ =>
        new Tuple1[S, T1, ReprT1](Targets[S], this, _1).connect()
    }

    def toString[S <: Sys[S]](_1: ReprT1[S]): String = s"$name${_1}"

    def name: String
  }

  final class Tuple2[S <: Sys[S], T1, ReprT1[~ <: Sys[~]] <: Expr[~, T1],
                                  T2, ReprT2[~ <: Sys[~]] <: Expr[~, T2]](
       protected val targets: Targets[S], val op: Tuple2Op[Boolean, T1, T2, BooleanObj, ReprT1, ReprT2],
       val _1: ReprT1[S], val _2: ReprT2[S])
    extends impl.Tuple2[S, Boolean, T1, T2, BooleanObj, ReprT1, ReprT2] with BooleanObj[S] {

    def tpe: Obj.Type = BooleanObj

    private[lucre] def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
      new Tuple2[Out, T1, ReprT1, T2, ReprT2](Targets[Out], op, context(_1), context(_2)).connect()
  }

  private[this] case object Not extends UnaryOp[Boolean, BooleanObj] {
    final val id = 0
    def value(a: Boolean): Boolean = !a
    def name = "!"
  }

  sealed trait BinaryOp[T1, ReprT1[~ <: Sys[~]] <: Expr[~, T1]]
    extends impl.Tuple2Op[Boolean, T1, T1, BooleanObj, ReprT1, ReprT1] {

    def name: String

    def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Ex[S]

    final def toString[S <: Sys[S]](_1: ReprT1[S], _2: ReprT1[S]): String = s"(${_1} $name ${_2})"
  }

  private[this] trait BooleanBinaryOp extends BinaryOp[Boolean, BooleanObj] { op =>
    def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Ex[S] = {
      val _1 = BooleanObj.read(in, access)
      val _2 = BooleanObj.read(in, access)
      new LazyTuple2[S](targets, op, _1, _2)
    }

    def isLazy: Boolean

    def lazyValue[S <: Sys[S]](_1: Ex[S], _2: Ex[S])(implicit tx: S#Tx): Boolean

    // def tryValue_1[S <: Sys[S]](_1: Boolean): Option[Boolean]

    final def apply[S <: Sys[S]](a: Ex[S], b: Ex[S])(implicit tx: S#Tx): Ex[S] = (a, b) match {
      case (Expr.Const(ca), Expr.Const(cb)) => BooleanObj.newConst(value(ca, cb))
      case _ => new LazyTuple2[S](Targets[S], this, a, b).connect()
    }
  }

  sealed trait IntBinaryOp extends BinaryOp[Int, IntObj] { op =>
    final def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Ex[S] = {
      val _1 = IntObj.read(in, access)
      val _2 = IntObj.read(in, access)
      new Tuple2[S, Int, IntObj, Int, IntObj](targets, op, _1, _2)
    }

    // ---- impl ----

    final def apply[S <: Sys[S]](a: IntObj[S], b: IntObj[S])(implicit tx: S#Tx): Ex[S] = (a, b) match {
      case (Expr.Const(ca), Expr.Const(cb)) => BooleanObj.newConst(value(ca, cb))
      case _ => new Tuple2[S, Int, IntObj, Int, IntObj](Targets[S], this, a, b).connect()
    }
  }

  private final class LazyTuple2[S <: Sys[S]](protected val targets: evt.Targets[S], op: BooleanBinaryOp,
                                              _1: Ex[S], _2: Ex[S])
    extends impl.NodeImpl[S, Boolean] with BooleanObj[S] {

    def tpe: Obj.Type = BooleanObj

    def connect()(implicit tx: S#Tx): this.type = {
      _1.changed ---> changed
      _2.changed ---> changed
      this
    }

    private[this] def disconnect()(implicit tx: S#Tx): Unit = {
      _1.changed -/-> changed
      _2.changed -/-> changed
    }

    protected def disposeData()(implicit tx: S#Tx): Unit = disconnect()

    def value(implicit tx: S#Tx): Boolean = op.lazyValue(_1, _2)

    private[lucre] def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
      new LazyTuple2(Targets[Out], op, context(_1), context(_2)).connect()

    protected def writeData(out: DataOutput): Unit = {
      out.writeByte(1)  // 'node not var'
      // out.writeInt(BooleanObj.typeID)
      out.writeInt(op.id)
      _1.write(out)
      _2.write(out)
    }

    object changed extends Changed {
      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Change[Boolean]] = {
        val _1c = _1.changed
        val _2c = _2.changed

        val _1ch = if (pull.contains(_1c)) pull(_1c) else None
        // yo crazy mama
        if (!_1ch.exists(_.isSignificant) && op.isLazy) return None

        val _2ch = if (pull.contains(_2c)) pull(_2c) else None

        (_1ch, _2ch) match {
          case (Some(ach), None) =>
            val bv      = _2.value
            val before  = op.value(ach.before, bv)
            val now     = op.value(ach.now, bv)
            if (before == now) None else Some(Change(before, now))
          case (None, Some(bch)) =>
            val av      = _1.value
            val before  = op.value(av, bch.before)
            val now     = op.value(av, bch.now)
            if (before == now) None else Some(Change(before, now))
          case (Some(ach), Some(bch)) =>
            val before  = op.value(ach.before, bch.before)
            val now     = op.value(ach.now, bch.now)
            if (before == now) None else Some(Change(before, now))
          case _ => None
        }
      }
    }

    override def toString: String = op.toString(_1, _2)
  }

  // ---- (Boolean, Boolean) => Boolean ----

  private[this] case object And extends BooleanBinaryOp {
    final val id = 1
    def value(a: Boolean, b: Boolean): Boolean = a && b
    def name = "&&"

    def lazyValue[S <: Sys[S]](_1: Ex[S], _2: Ex[S])(implicit tx: S#Tx): Boolean =
      _1.value && _2.value

    def isLazy = true

    // def tryValue_1[S <: Sys[S]](_1: Boolean): Option[Boolean] = if (_1) None else Some(false)
  }

  private[this] case object Or extends BooleanBinaryOp {
    final val id = 2
    def value(a: Boolean, b: Boolean): Boolean = a || b
    def name = "||"

    def lazyValue[S <: Sys[S]](_1: Ex[S], _2: Ex[S])(implicit tx: S#Tx): Boolean =
      _1.value || _2.value

    def isLazy = true

    // def tryValue_1[S <: Sys[S]](_1: Boolean): Option[Boolean] = if (_1) Some(true) else None
  }

  private[this] case object Xor extends BooleanBinaryOp {
    final val id = 3
    def value(a: Boolean, b: Boolean): Boolean = a ^ b
    def name = "^"

    def lazyValue[S <: Sys[S]](_1: Ex[S], _2: Ex[S])(implicit tx: S#Tx): Boolean =
      _1.value ^ _2.value   // eager actually

    def isLazy = false

    // def tryValue_1[S <: Sys[S]](_1: Boolean): Option[Boolean] = None
  }

  // ---- (Int, Int) => Boolean ----

  case object IntEq extends IntBinaryOp {
    final val id = 4
    def value(a: Int, b: Int): Boolean = a == b
    def name = "sig_=="
  }

  case object IntNeq extends IntBinaryOp {
    final val id = 5
    def value(a: Int, b: Int): Boolean = a != b
    def name = "sig_!="
  }

  case object IntLt extends IntBinaryOp {
    final val id = 6
    def value(a: Int, b: Int): Boolean = a < b
    def name = "<"
  }

  case object IntGt extends IntBinaryOp {
    final val id = 7
    def value(a: Int, b: Int): Boolean = a > b
    def name = ">"
  }

  case object IntLeq extends IntBinaryOp {
    final val id = 8
    def value(a: Int, b: Int): Boolean = a <= b
    def name = "<="
  }

  case object IntGeq extends IntBinaryOp {
    final val id = 9
    def value(a: Int, b: Int): Boolean = a >= b
    def name = ">="
  }
}