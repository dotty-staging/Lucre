/*
 *  Primitives.scala
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
import de.sciss.lucre.{Var => LVar}
import de.sciss.serial.{ConstFormat, TFormat}
import de.sciss.span.{Span, SpanLike}

import scala.collection.immutable.{IndexedSeq => Vec}

trait IntObj      [T <: Txn[T]] extends Expr[T, Int        ]
trait LongObj     [T <: Txn[T]] extends Expr[T, Long       ]
trait DoubleObj   [T <: Txn[T]] extends Expr[T, Double     ]
trait BooleanObj  [T <: Txn[T]] extends Expr[T, Boolean    ]
trait StringObj   [T <: Txn[T]] extends Expr[T, String     ]
trait SpanLikeObj [T <: Txn[T]] extends Expr[T, SpanLike   ]
trait SpanObj     [T <: Txn[T]] extends Expr[T, Span       ]
trait IntVector   [T <: Txn[T]] extends Expr[T, Vec[Int   ]]
trait DoubleVector[T <: Txn[T]] extends Expr[T, Vec[Double]]

object IntObj extends impl.ExprTypeImpl[Int, IntObj] {
  import lucre.{IntObj => Repr}

  final val typeId = 2
  final val valueFormat = TFormat.Int

  override def toString = "IntObj"

  final val valueName = "Int"

  def tryParse(value: Any): Option[Int] = value match {
    case x: Int => Some(x)
    case _      => None
  }

  override protected def mkConst[T <: Txn[T]](id: Ident[T], value: A)(implicit tx: T): Const[T] =
    new _Const[T](id, value)

  override protected def mkVar[T <: Txn[T]](targets: Event.Targets[T], vr: LVar[T, E[T]], connect: Boolean)
                                           (implicit tx: T): Var[T] = {
    val res = new _Var[T](targets, vr)
    if (connect) res.connect()
    res
  }

  override protected def mkProgram[T <: Txn[T]](targets: Event.Targets[T], program: LVar[T, Ex[A]],
                                                sources: LVar[T, Vec[Event[T, Any]]], value: LVar[T, A],
                                                connect: Boolean)(implicit tx: T): Program[T] = {
    val res = new _Program[T](targets, programRef = program, sourcesRef = sources, valueRef = value)
    if (connect) res.connect()
    res
  }

  private[this] final class _Const[T <: Txn[T]](val id: Ident[T], val constValue: A)
    extends ConstImpl[T] with Repr[T]

  private[this] final class _Var[T <: Txn[T]](val targets: Event.Targets[T],
                                              val ref: LVar[T, E[T]])
    extends VarImpl[T] with Repr[T]

  private[this] final class _Program[T <: Txn[T]](val targets   : Event.Targets[T],
                                                  val programRef: LVar[T, Ex[A]],
                                                  val sourcesRef: LVar[T, Vec[Event[T, Any]]],
                                                  val valueRef  : LVar[T, A]
                                                 )
    extends ProgramImpl[T] with Repr[T]
}

object LongObj extends impl.ExprTypeImpl[Long, LongObj] {
  import lucre.{LongObj => Repr}

  final val typeId = 3
  final val valueFormat = TFormat.Long

  final val valueName = "Long"

  override def toString = "LongObj"

  def tryParse(value: Any): Option[Long] = value match {
    case x: Long  => Some(x)
    case x: Int   => Some(x.toLong)
    case _        => None
  }

  override protected def mkConst[T <: Txn[T]](id: Ident[T], value: A)(implicit tx: T): Const[T] =
    new _Const[T](id, value)

  override protected def mkVar[T <: Txn[T]](targets: Event.Targets[T], vr: LVar[T, E[T]], connect: Boolean)
                                           (implicit tx: T): Var[T] = {
    val res = new _Var[T](targets, vr)
    if (connect) res.connect()
    res
  }

  override protected def mkProgram[T <: Txn[T]](targets: Event.Targets[T], program: LVar[T, Ex[A]],
                                                sources: LVar[T, Vec[Event[T, Any]]], value: LVar[T, A],
                                                connect: Boolean)(implicit tx: T): Program[T] = {
    val res = new _Program[T](targets, programRef = program, sourcesRef = sources, valueRef = value)
    if (connect) res.connect()
    res
  }

  private[this] final class _Const[T <: Txn[T]](val id: Ident[T], val constValue: A)
    extends ConstImpl[T] with Repr[T]

  private[this] final class _Var[T <: Txn[T]](val targets: Event.Targets[T],
                                              val ref: LVar[T, E[T]])
    extends VarImpl[T] with Repr[T]

  private[this] final class _Program[T <: Txn[T]](val targets   : Event.Targets[T],
                                                  val programRef: LVar[T, Ex[A]],
                                                  val sourcesRef: LVar[T, Vec[Event[T, Any]]],
                                                  val valueRef  : LVar[T, A]
                                                 )
    extends ProgramImpl[T] with Repr[T]
}

object DoubleObj extends impl.ExprTypeImpl[Double, DoubleObj] {
  import lucre.{DoubleObj => Repr}

  final val typeId = 5
  final val valueFormat = TFormat.Double

  final val valueName = "Double"

  override def toString = "DoubleObj"

  def tryParse(in: Any): Option[Double] = in match {
    case d: Double  => Some(d)
    case f: Float   => Some(f.toDouble)
    case i: Int     => Some(i.toDouble)
    case _          => None
  }

  override protected def mkConst[T <: Txn[T]](id: Ident[T], value: A)(implicit tx: T): Const[T] =
    new _Const[T](id, value)

  override protected def mkVar[T <: Txn[T]](targets: Event.Targets[T], vr: LVar[T, E[T]], connect: Boolean)
                                           (implicit tx: T): Var[T] = {
    val res = new _Var[T](targets, vr)
    if (connect) res.connect()
    res
  }

  override protected def mkProgram[T <: Txn[T]](targets: Event.Targets[T], program: LVar[T, Ex[A]],
                                                sources: LVar[T, Vec[Event[T, Any]]], value: LVar[T, A],
                                                connect: Boolean)(implicit tx: T): Program[T] = {
    val res = new _Program[T](targets, programRef = program, sourcesRef = sources, valueRef = value)
    if (connect) res.connect()
    res
  }

  private[this] final class _Const[T <: Txn[T]](val id: Ident[T], val constValue: A)
    extends ConstImpl[T] with Repr[T]

  private[this] final class _Var[T <: Txn[T]](val targets: Event.Targets[T],
                                              val ref: LVar[T, E[T]])
    extends VarImpl[T] with Repr[T]

  private[this] final class _Program[T <: Txn[T]](val targets   : Event.Targets[T],
                                                  val programRef: LVar[T, Ex[A]],
                                                  val sourcesRef: LVar[T, Vec[Event[T, Any]]],
                                                  val valueRef  : LVar[T, A]
                                                 )
    extends ProgramImpl[T] with Repr[T]
}

object BooleanObj extends impl.ExprTypeImpl[Boolean, BooleanObj] {
  import lucre.{BooleanObj => Repr}

  final val typeId = 6
  final val valueFormat = TFormat.Boolean

  final val valueName = "Boolean"

  override def toString = "BooleanObj"

  def tryParse(in: Any): Option[Boolean] = in match {
    case x: Boolean => Some(x)
    case _          => None
  }

  override protected def mkConst[T <: Txn[T]](id: Ident[T], value: A)(implicit tx: T): Const[T] =
    new _Const[T](id, value)

  override protected def mkVar[T <: Txn[T]](targets: Event.Targets[T], vr: LVar[T, E[T]], connect: Boolean)
                                           (implicit tx: T): Var[T] = {
    val res = new _Var[T](targets, vr)
    if (connect) res.connect()
    res
  }

  override protected def mkProgram[T <: Txn[T]](targets: Event.Targets[T], program: LVar[T, Ex[A]],
                                                sources: LVar[T, Vec[Event[T, Any]]], value: LVar[T, A],
                                                connect: Boolean)(implicit tx: T): Program[T] = {
    val res = new _Program[T](targets, programRef = program, sourcesRef = sources, valueRef = value)
    if (connect) res.connect()
    res
  }

  private[this] final class _Const[T <: Txn[T]](val id: Ident[T], val constValue: A)
    extends ConstImpl[T] with Repr[T]

  private[this] final class _Var[T <: Txn[T]](val targets: Event.Targets[T],
                                              val ref: LVar[T, E[T]])
    extends VarImpl[T] with Repr[T]

  private[this] final class _Program[T <: Txn[T]](val targets   : Event.Targets[T],
                                                  val programRef: LVar[T, Ex[A]],
                                                  val sourcesRef: LVar[T, Vec[Event[T, Any]]],
                                                  val valueRef  : LVar[T, A]
                                                 )
    extends ProgramImpl[T] with Repr[T]
}

object StringObj extends impl.ExprTypeImpl[String, StringObj] {
  import lucre.{StringObj => Repr}

  final val typeId = 8
  final val valueFormat = TFormat.String

  final val valueName = "String"

  override def toString = "StringObj"

  def tryParse(in: Any): Option[String] = in match {
    case x: String  => Some(x)
    case _          => None
  }

  override protected def mkConst[T <: Txn[T]](id: Ident[T], value: A)(implicit tx: T): Const[T] =
    new _Const[T](id, value)

  override protected def mkVar[T <: Txn[T]](targets: Event.Targets[T], vr: LVar[T, E[T]], connect: Boolean)
                                           (implicit tx: T): Var[T] = {
    val res = new _Var[T](targets, vr)
    if (connect) res.connect()
    res
  }

  override protected def mkProgram[T <: Txn[T]](targets: Event.Targets[T], program: LVar[T, Ex[A]],
                                                sources: LVar[T, Vec[Event[T, Any]]], value: LVar[T, A],
                                                connect: Boolean)(implicit tx: T): Program[T] = {
    val res = new _Program[T](targets, programRef = program, sourcesRef = sources, valueRef = value)
    if (connect) res.connect()
    res
  }

  private[this] final class _Const[T <: Txn[T]](val id: Ident[T], val constValue: A)
    extends ConstImpl[T] with Repr[T]

  private[this] final class _Var[T <: Txn[T]](val targets: Event.Targets[T],
                                              val ref: LVar[T, E[T]])
    extends VarImpl[T] with Repr[T]

  private[this] final class _Program[T <: Txn[T]](val targets   : Event.Targets[T],
                                                  val programRef: LVar[T, Ex[A]],
                                                  val sourcesRef: LVar[T, Vec[Event[T, Any]]],
                                                  val valueRef  : LVar[T, A]
                                                 )
    extends ProgramImpl[T] with Repr[T]
}

object SpanLikeObj extends impl.ExprTypeImpl[SpanLike, SpanLikeObj] {
  import lucre.{SpanLikeObj => Repr}

  final val typeId = 9
  final val valueFormat = SpanLike.format

  final val valueName = "SpanLike"

  override def toString = "SpanLikeObj"

  def tryParse(in: Any): Option[SpanLike] = in match {
    case x: SpanLike  => Some(x)
    case _            => None
  }

  override protected def mkConst[T <: Txn[T]](id: Ident[T], value: A)(implicit tx: T): Const[T] =
    new _Const[T](id, value)

  override protected def mkVar[T <: Txn[T]](targets: Event.Targets[T], vr: LVar[T, E[T]], connect: Boolean)
                                           (implicit tx: T): Var[T] = {
    val res = new _Var[T](targets, vr)
    if (connect) res.connect()
    res
  }

  override protected def mkProgram[T <: Txn[T]](targets: Event.Targets[T], program: LVar[T, Ex[A]],
                                                sources: LVar[T, Vec[Event[T, Any]]], value: LVar[T, A],
                                                connect: Boolean)(implicit tx: T): Program[T] = {
    val res = new _Program[T](targets, programRef = program, sourcesRef = sources, valueRef = value)
    if (connect) res.connect()
    res
  }

  private[this] final class _Const[T <: Txn[T]](val id: Ident[T], val constValue: A)
    extends ConstImpl[T] with Repr[T]

  private[this] final class _Var[T <: Txn[T]](val targets: Event.Targets[T],
                                              val ref: LVar[T, E[T]])
    extends VarImpl[T] with Repr[T]

  private[this] final class _Program[T <: Txn[T]](val targets   : Event.Targets[T],
                                                  val programRef: LVar[T, Ex[A]],
                                                  val sourcesRef: LVar[T, Vec[Event[T, Any]]],
                                                  val valueRef  : LVar[T, A]
                                                 )
    extends ProgramImpl[T] with Repr[T]
}

object SpanObj extends impl.ExprTypeImpl[Span, SpanObj] {
  import lucre.{SpanObj => Repr}

  final val typeId = 10
  final val valueFormat = Span.format

  final val valueName = "Span"

  override def toString = "SpanObj"

  def tryParse(in: Any): Option[Span] = in match {
    case x: Span  => Some(x)
    case _        => None
  }

  override protected def mkConst[T <: Txn[T]](id: Ident[T], value: A)(implicit tx: T): Const[T] =
    new _Const[T](id, value)

  override protected def mkVar[T <: Txn[T]](targets: Event.Targets[T], vr: LVar[T, E[T]], connect: Boolean)
                                           (implicit tx: T): Var[T] = {
    val res = new _Var[T](targets, vr)
    if (connect) res.connect()
    res
  }

  override protected def mkProgram[T <: Txn[T]](targets: Event.Targets[T], program: LVar[T, Ex[A]],
                                                sources: LVar[T, Vec[Event[T, Any]]], value: LVar[T, A],
                                                connect: Boolean)(implicit tx: T): Program[T] = {
    val res = new _Program[T](targets, programRef = program, sourcesRef = sources, valueRef = value)
    if (connect) res.connect()
    res
  }

  private[this] final class _Const[T <: Txn[T]](val id: Ident[T], val constValue: A)
    extends ConstImpl[T] with Repr[T]

  private[this] final class _Var[T <: Txn[T]](val targets: Event.Targets[T],
                                              val ref: LVar[T, E[T]])
    extends VarImpl[T] with Repr[T]

  private[this] final class _Program[T <: Txn[T]](val targets   : Event.Targets[T],
                                                  val programRef: LVar[T, Ex[A]],
                                                  val sourcesRef: LVar[T, Vec[Event[T, Any]]],
                                                  val valueRef  : LVar[T, A]
                                                 )
    extends ProgramImpl[T] with Repr[T]
}

object IntVector extends impl.ExprTypeImpl[Vec[Int], IntVector] {
  import lucre.{IntVector => Repr}

  final val typeId = 0x2002 //  0x2000 | IntObj.typeId
  final val valueFormat: ConstFormat[Vec[Int]] = ConstFormat.vec

  final val valueName = "Seq[Int]"

  override def toString = "IntVector"

  def tryParse(in: Any): Option[Vec[Int]] = in match {
    case xs: Vec[Any] =>
      val ok = xs.forall {
        case _: Int => true
        case _      => false
      }
      if (ok) Some(xs.asInstanceOf[Vec[Int]]) else None

    case _ => None
  }

  override protected def mkConst[T <: Txn[T]](id: Ident[T], value: A)(implicit tx: T): Const[T] =
    new _Const[T](id, value)

  override protected def mkVar[T <: Txn[T]](targets: Event.Targets[T], vr: LVar[T, E[T]], connect: Boolean)
                                           (implicit tx: T): Var[T] = {
    val res = new _Var[T](targets, vr)
    if (connect) res.connect()
    res
  }

  override protected def mkProgram[T <: Txn[T]](targets: Event.Targets[T], program: LVar[T, Ex[A]],
                                                sources: LVar[T, Vec[Event[T, Any]]], value: LVar[T, A],
                                                connect: Boolean)(implicit tx: T): Program[T] = {
    val res = new _Program[T](targets, programRef = program, sourcesRef = sources, valueRef = value)
    if (connect) res.connect()
    res
  }

  private[this] final class _Const[T <: Txn[T]](val id: Ident[T], val constValue: A)
    extends ConstImpl[T] with Repr[T]

  private[this] final class _Var[T <: Txn[T]](val targets: Event.Targets[T],
                                              val ref: LVar[T, E[T]])
    extends VarImpl[T] with Repr[T]

  private[this] final class _Program[T <: Txn[T]](val targets   : Event.Targets[T],
                                                  val programRef: LVar[T, Ex[A]],
                                                  val sourcesRef: LVar[T, Vec[Event[T, Any]]],
                                                  val valueRef  : LVar[T, A]
                                                 )
    extends ProgramImpl[T] with Repr[T]
}

object DoubleVector extends impl.ExprTypeImpl[Vec[Double], DoubleVector] {
  import lucre.{DoubleVector => Repr}

  final val typeId = 0x2005 //  0x2000 | DoubleObj.typeId
  final val valueFormat: ConstFormat[Vec[Double]] = ConstFormat.vec

  final val valueName = "Seq[Double]"

  override def toString = "DoubleVector"

  def tryParse(in: Any): Option[Vec[Double]] = in match {
    case xs: Vec[Any] =>
      val ok = xs.forall {
        case _: Double  => true  // don't bother looking for `Float` now
        case _          => false
      }
      if (ok) Some(xs.asInstanceOf[Vec[Double]]) else None

    case _ => None
  }

  override protected def mkConst[T <: Txn[T]](id: Ident[T], value: A)(implicit tx: T): Const[T] =
    new _Const[T](id, value)

  override protected def mkVar[T <: Txn[T]](targets: Event.Targets[T], vr: LVar[T, E[T]], connect: Boolean)
                                           (implicit tx: T): Var[T] = {
    val res = new _Var[T](targets, vr)
    if (connect) res.connect()
    res
  }

  override protected def mkProgram[T <: Txn[T]](targets: Event.Targets[T], program: LVar[T, Ex[A]],
                                                sources: LVar[T, Vec[Event[T, Any]]], value: LVar[T, A],
                                                connect: Boolean)(implicit tx: T): Program[T] = {
    val res = new _Program[T](targets, programRef = program, sourcesRef = sources, valueRef = value)
    if (connect) res.connect()
    res
  }

  private[this] final class _Const[T <: Txn[T]](val id: Ident[T], val constValue: A)
    extends ConstImpl[T] with Repr[T]

  private[this] final class _Var[T <: Txn[T]](val targets: Event.Targets[T],
                                              val ref: LVar[T, E[T]])
    extends VarImpl[T] with Repr[T]

  private[this] final class _Program[T <: Txn[T]](val targets   : Event.Targets[T],
                                                  val programRef: LVar[T, Ex[A]],
                                                  val sourcesRef: LVar[T, Vec[Event[T, Any]]],
                                                  val valueRef  : LVar[T, A]
                                                 )
    extends ProgramImpl[T] with Repr[T]
}
