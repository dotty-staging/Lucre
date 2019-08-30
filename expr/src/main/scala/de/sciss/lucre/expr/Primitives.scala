/*
 *  Primitives.scala
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
import de.sciss.lucre.expr
import de.sciss.lucre.stm.Sys
import de.sciss.serial.{ImmutableSerializer, Serializer}
import de.sciss.span.{Span, SpanLike}

import scala.collection.immutable.{IndexedSeq => Vec}

trait IntObj      [S <: Sys[S]] extends Expr[S, Int        ]
trait LongObj     [S <: Sys[S]] extends Expr[S, Long       ]
trait DoubleObj   [S <: Sys[S]] extends Expr[S, Double     ]
trait BooleanObj  [S <: Sys[S]] extends Expr[S, Boolean    ]
trait StringObj   [S <: Sys[S]] extends Expr[S, String     ]
trait SpanLikeObj [S <: Sys[S]] extends Expr[S, SpanLike   ]
trait SpanObj     [S <: Sys[S]] extends Expr[S, Span       ]
trait IntVector   [S <: Sys[S]] extends Expr[S, Vec[Int   ]]
trait DoubleVector[S <: Sys[S]] extends Expr[S, Vec[Double]]

object IntObj extends impl.ExprTypeImpl[Int, IntObj] {
  import expr.{IntObj => Repr}

  final val typeId = 2
  final val valueSerializer = Serializer.Int

  override def toString = "IntObj"

  def tryParse(value: Any): Option[Int] = value match {
    case x: Int => Some(x)
    case _      => None
  }

  protected def mkConst[S <: Sys[S]](id: S#Id, value: A)(implicit tx: S#Tx): Const[S] =
    new _Const[S](id, value)

  protected def mkVar[S <: Sys[S]](targets: Targets[S], vr: S#Var[_Ex[S]], connect: Boolean)
                                  (implicit tx: S#Tx): Var[S] = {
    val res = new _Var[S](targets, vr)
    if (connect) res.connect()
    res
  }

  private[this] final class _Const[S <: Sys[S]](val id: S#Id, val constValue: A)
    extends ConstImpl[S] with Repr[S]

  private[this] final class _Var[S <: Sys[S]](val targets: Targets[S], val ref: S#Var[_Ex[S]])
    extends VarImpl[S] with Repr[S]
}

object LongObj extends impl.ExprTypeImpl[Long, LongObj] {
  import expr.{LongObj => Repr}

  final val typeId = 3
  final val valueSerializer = Serializer.Long

  override def toString = "LongObj"

  def tryParse(value: Any): Option[Long] = value match {
    case x: Long  => Some(x)
    case x: Int   => Some(x.toLong)
    case _        => None
  }

  protected def mkConst[S <: Sys[S]](id: S#Id, value: A)(implicit tx: S#Tx): Const[S] =
    new _Const[S](id, value)

  protected def mkVar[S <: Sys[S]](targets: Targets[S], vr: S#Var[_Ex[S]], connect: Boolean)
                                  (implicit tx: S#Tx): Var[S] = {
    val res = new _Var[S](targets, vr)
    if (connect) res.connect()
    res
  }

  private[this] final class _Const[S <: Sys[S]](val id: S#Id, val constValue: A)
    extends ConstImpl[S] with Repr[S]

  private[this] final class _Var[S <: Sys[S]](val targets: Targets[S], val ref: S#Var[_Ex[S]])
    extends VarImpl[S] with Repr[S]
}

object DoubleObj extends impl.ExprTypeImpl[Double, DoubleObj] {
  import expr.{DoubleObj => Repr}

  final val typeId = 5
  final val valueSerializer = Serializer.Double

  override def toString = "DoubleObj"

  def tryParse(in: Any): Option[Double] = in match {
    case d: Double  => Some(d)
    case f: Float   => Some(f.toDouble)
    case i: Int     => Some(i.toDouble)
    case _          => None
  }

  protected def mkConst[S <: Sys[S]](id: S#Id, value: A)(implicit tx: S#Tx): Const[S] =
    new _Const[S](id, value)

  protected def mkVar[S <: Sys[S]](targets: Targets[S], vr: S#Var[_Ex[S]], connect: Boolean)
                                  (implicit tx: S#Tx): Var[S] = {
    val res = new _Var[S](targets, vr)
    if (connect) res.connect()
    res
  }

  private[this] final class _Const[S <: Sys[S]](val id: S#Id, val constValue: A)
    extends ConstImpl[S] with Repr[S]

  private[this] final class _Var[S <: Sys[S]](val targets: Targets[S], val ref: S#Var[_Ex[S]])
    extends VarImpl[S] with Repr[S]
}

object BooleanObj extends impl.ExprTypeImpl[Boolean, BooleanObj] {
  import expr.{BooleanObj => Repr}

  final val typeId = 6
  final val valueSerializer = Serializer.Boolean

  override def toString = "BooleanObj"

  def tryParse(in: Any): Option[Boolean] = in match {
    case x: Boolean => Some(x)
    case _          => None
  }

  protected def mkConst[S <: Sys[S]](id: S#Id, value: A)(implicit tx: S#Tx): Const[S] =
    new _Const[S](id, value)

  protected def mkVar[S <: Sys[S]](targets: Targets[S], vr: S#Var[_Ex[S]], connect: Boolean)
                                  (implicit tx: S#Tx): Var[S] = {
    val res = new _Var[S](targets, vr)
    if (connect) res.connect()
    res
  }

  private[this] final class _Const[S <: Sys[S]](val id: S#Id, val constValue: A)
    extends ConstImpl[S] with Repr[S]

  private[this] final class _Var[S <: Sys[S]](val targets: Targets[S], val ref: S#Var[_Ex[S]])
    extends VarImpl[S] with Repr[S]
}

object StringObj extends impl.ExprTypeImpl[String, StringObj] {
  import expr.{StringObj => Repr}

  final val typeId = 8
  final val valueSerializer = Serializer.String

  override def toString = "StringObj"

  def tryParse(in: Any): Option[String] = in match {
    case x: String  => Some(x)
    case _          => None
  }

  protected def mkConst[S <: Sys[S]](id: S#Id, value: A)(implicit tx: S#Tx): Const[S] =
    new _Const[S](id, value)

  protected def mkVar[S <: Sys[S]](targets: Targets[S], vr: S#Var[_Ex[S]], connect: Boolean)
                                  (implicit tx: S#Tx): Var[S] = {
    val res = new _Var[S](targets, vr)
    if (connect) res.connect()
    res
  }

  private[this] final class _Const[S <: Sys[S]](val id: S#Id, val constValue: A)
    extends ConstImpl[S] with Repr[S]

  private[this] final class _Var[S <: Sys[S]](val targets: Targets[S], val ref: S#Var[_Ex[S]])
    extends VarImpl[S] with Repr[S]
}

object SpanLikeObj extends impl.ExprTypeImpl[SpanLike, SpanLikeObj] {
  import expr.{SpanLikeObj => Repr}

  final val typeId = 9
  final val valueSerializer: ImmutableSerializer[SpanLike] = SpanLike.serializer

  override def toString = "SpanLikeObj"

  def tryParse(in: Any): Option[SpanLike] = in match {
    case x: SpanLike  => Some(x)
    case _            => None
  }

  protected def mkConst[S <: Sys[S]](id: S#Id, value: A)(implicit tx: S#Tx): Const[S] =
    new _Const[S](id, value)

  protected def mkVar[S <: Sys[S]](targets: Targets[S], vr: S#Var[_Ex[S]], connect: Boolean)
                                  (implicit tx: S#Tx): Var[S] = {
    val res = new _Var[S](targets, vr)
    if (connect) res.connect()
    res
  }

  private[this] final class _Const[S <: Sys[S]](val id: S#Id, val constValue: A)
    extends ConstImpl[S] with Repr[S]

  private[this] final class _Var[S <: Sys[S]](val targets: Targets[S], val ref: S#Var[_Ex[S]])
    extends VarImpl[S] with Repr[S]
}

object SpanObj extends impl.ExprTypeImpl[Span, SpanObj] {
  import expr.{SpanObj => Repr}

  final val typeId = 10
  final val valueSerializer: ImmutableSerializer[Span] = Span.serializer

  override def toString = "SpanObj"

  def tryParse(in: Any): Option[Span] = in match {
    case x: Span  => Some(x)
    case _        => None
  }

  protected def mkConst[S <: Sys[S]](id: S#Id, value: A)(implicit tx: S#Tx): Const[S] =
    new _Const[S](id, value)

  protected def mkVar[S <: Sys[S]](targets: Targets[S], vr: S#Var[_Ex[S]], connect: Boolean)
                                  (implicit tx: S#Tx): Var[S] = {
    val res = new _Var[S](targets, vr)
    if (connect) res.connect()
    res
  }

  private[this] final class _Const[S <: Sys[S]](val id: S#Id, val constValue: A)
    extends ConstImpl[S] with Repr[S]

  private[this] final class _Var[S <: Sys[S]](val targets: Targets[S], val ref: S#Var[_Ex[S]])
    extends VarImpl[S] with Repr[S]
}

object IntVector extends impl.ExprTypeImpl[Vec[Int], IntVector] {
  import expr.{IntVector => Repr}

  final val typeId = 0x2002 //  0x2000 | IntObj.typeId
  final val valueSerializer: ImmutableSerializer[Vec[Int]] = ImmutableSerializer.indexedSeq

  override def toString = "IntVector"

  def tryParse(in: Any): Option[Vec[Int]] = in match {
    case xs: Vec[_] =>
      val ok = xs.forall {
        case _: Int => true
      }
      if (ok) Some(xs.asInstanceOf[Vec[Int]]) else None

    case _ => None
  }

  protected def mkConst[S <: Sys[S]](id: S#Id, value: A)(implicit tx: S#Tx): Const[S] =
    new _Const[S](id, value)

  protected def mkVar[S <: Sys[S]](targets: Targets[S], vr: S#Var[_Ex[S]], connect: Boolean)
                                  (implicit tx: S#Tx): Var[S] = {
    val res = new _Var[S](targets, vr)
    if (connect) res.connect()
    res
  }

  private[this] final class _Const[S <: Sys[S]](val id: S#Id, val constValue: A)
    extends ConstImpl[S] with Repr[S]

  private[this] final class _Var[S <: Sys[S]](val targets: Targets[S], val ref: S#Var[_Ex[S]])
    extends VarImpl[S] with Repr[S]
}

object DoubleVector extends impl.ExprTypeImpl[Vec[Double], DoubleVector] {
  import expr.{DoubleVector => Repr}

  final val typeId = 0x2005 //  0x2000 | DoubleObj.typeId
  final val valueSerializer: ImmutableSerializer[Vec[Double]] = ImmutableSerializer.indexedSeq

  override def toString = "DoubleVector"

  def tryParse(in: Any): Option[Vec[Double]] = in match {
    case xs: Vec[_] =>
      val ok = xs.forall {
        case _: Double => true  // don't bother looking for `Float` now
      }
      if (ok) Some(xs.asInstanceOf[Vec[Double]]) else None

    case _ => None
  }

  protected def mkConst[S <: Sys[S]](id: S#Id, value: A)(implicit tx: S#Tx): Const[S] =
    new _Const[S](id, value)

  protected def mkVar[S <: Sys[S]](targets: Targets[S], vr: S#Var[_Ex[S]], connect: Boolean)
                                  (implicit tx: S#Tx): Var[S] = {
    val res = new _Var[S](targets, vr)
    if (connect) res.connect()
    res
  }

  private[this] final class _Const[S <: Sys[S]](val id: S#Id, val constValue: A)
    extends ConstImpl[S] with Repr[S]

  private[this] final class _Var[S <: Sys[S]](val targets: Targets[S], val ref: S#Var[_Ex[S]])
    extends VarImpl[S] with Repr[S]
}
