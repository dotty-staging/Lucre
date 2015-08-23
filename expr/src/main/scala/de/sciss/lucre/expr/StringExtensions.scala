/*
 *  StringExtensions.scala
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
import de.sciss.lucre.expr.impl.Tuple2Op
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.serial.DataInput

import scala.language.higherKinds

object StringExtensions  {
  private[this] lazy val _init: Unit = {
    StringObj.registerExtension(StringTuple2s)
  }

  def init(): Unit = _init

  type Ex[S <: Sys[S]] = StringObj[S]

  private[this] object StringTuple2s extends Type.Extension1[StringObj] {
    // final val arity = 2
    final val opLo  = BinaryOp.Append.id
    final val opHi  = BinaryOp.Append.id

    val name = "String-2 Ops"

    def readExtension[S <: Sys[S]](opID: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                                  (implicit tx: S#Tx): Ex[S] = {
      import BinaryOp._
      val op: Op = opID /* : @switch */ match {
        case Append.id => Append
      }
      val _1 = StringObj.read(in, access)
      val _2 = StringObj.read(in, access)
      new Tuple2(targets, op, _1, _2)
    }
  }

  final class Tuple2[S <: Sys[S], T1, ReprT1[~ <: Sys[~]] <: Expr[~, T1],
                                  T2, ReprT2[~ <: Sys[~]] <: Expr[~, T2]](
      protected val targets: Targets[S], val op: Tuple2Op[String, T1, T2, StringObj, ReprT1, ReprT2],
      val _1: ReprT1[S], val _2: ReprT2[S])
    extends impl.Tuple2[S, String, T1, T2, StringObj, ReprT1, ReprT2] with StringObj[S] {

    def tpe: Obj.Type = StringObj
  }

  // ----- operators -----

  final class Ops[S <: Sys[S]](val `this`: Ex[S]) extends AnyVal { me =>
    import me.{`this` => ex}
    private type E = Ex[S]

    import BinaryOp._

    def ++(b: E)(implicit tx: S#Tx): E = Append(ex, b)
  }

  private object BinaryOp {
    sealed abstract class Op extends impl.Tuple2Op[String, String, String, StringObj, StringObj, StringObj] {
      def id: Int
      final def apply[S <: Sys[S]](a: Ex[S], b: Ex[S])(implicit tx: S#Tx): Ex[S] = (a, b) match {
        case (Expr.Const(ca), Expr.Const(cb)) => StringObj.newConst(value(ca, cb))
        case _  =>
          new Tuple2(Targets[S], this, a, b).connect()
      }

      def value(a: String, b: String): String

      def toString[S <: Sys[S]](_1: Ex[S], _2: Ex[S]): String = s"${_1}.$name(${_2})"

      def name: String = {
        val cn = getClass.getName
        val sz = cn.length
        val i = cn.indexOf('$') + 1
        s"${cn.charAt(i).toLower}${cn.substring(i + 1, if (cn.charAt(sz - 1) == '$') sz - 1 else sz)}"
      }
    }

    case object Append extends Op {
      final val id = 0
      def value(a: String, b: String): String = a + b
    }
  }
}