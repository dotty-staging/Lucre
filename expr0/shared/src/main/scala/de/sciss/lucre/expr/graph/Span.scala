/*
 *  Span.scala
 *  (Lucre 4)
 *
 *  Copyright (c) 2009-2022 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr.graph

import de.sciss.lucre.expr.ExElem.{ProductReader, RefMapIn}
import de.sciss.lucre.expr.{Context, graph}
import de.sciss.lucre.{IExpr, Txn}
import de.sciss.span.{Span => _Span, SpanLike => _SpanLike}

object Span extends ProductReader[Ex[_Span]] {
  def apply(start: Ex[Long], stop: Ex[Long]): Ex[_Span] = Apply(start, stop)

  def all : Ex[_SpanLike] = All ()
  def void: Ex[_SpanLike] = Void()

  def from  (start: Ex[Long]): Ex[_SpanLike] = From (start)
  def until (stop : Ex[Long]): Ex[_SpanLike] = Until(stop)

  object All extends ProductReader[All] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): All = {
      require (arity == 0 && adj == 0)
      new All
    }
  }
  final case class All() extends Ex[_SpanLike] {
    override def productPrefix: String = s"Span$$All" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, _SpanLike]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
      new Const.Expanded(_Span.All)
  }

  object Void extends ProductReader[Void] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Void = {
      require (arity == 0 && adj == 0)
      new Void
    }
  }
  final case class Void() extends Ex[_SpanLike] {
    override def productPrefix: String = s"Span$$All" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, _SpanLike]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
      new Const.Expanded(_Span.Void)
  }

  // Note: only used when expanded, thus does not need deserialization
  private[lucre] final case class ApplyOp() extends BinaryOp.Op[Long, Long, _Span] {
    override def productPrefix: String = s"Span$$ApplyOp" // serialization

    def apply(a: Long, b: Long): _Span = _Span(a, b)
  }

  private final case class Apply(start: Ex[Long], stop: Ex[Long]) extends Ex[_Span] {
    override def productPrefix: String = "Span" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, _Span]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new graph.BinaryOp.Expanded(ApplyOp(), start.expand[T], stop.expand[T], tx)
    }
  }

  // Note: only used when expanded, thus does not need deserialization
  private[lucre] final case class FromOp() extends UnaryOp.Op[Long, _SpanLike] {
    override def productPrefix: String = s"Span$$FromOp" // serialization

    def apply(a: Long): _SpanLike = _Span.From(a)
  }

  // Note: only used when expanded, thus does not need deserialization
  private[lucre] final case class UntilOp() extends UnaryOp.Op[Long, _SpanLike] {
    override def productPrefix: String = s"Span$$UntilOp" // serialization

    def apply(a: Long): _SpanLike = _Span.Until(a)
  }

  object From extends ProductReader[From] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): From = {
      require (arity == 1 && adj == 0)
      val _start = in.readEx[Long]()
      new From(_start)
    }
  }
  case class From(start: Ex[Long]) extends Ex[_SpanLike] {
    override def productPrefix: String = s"Span$$From" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, _SpanLike]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new graph.UnaryOp.Expanded(FromOp(), start.expand[T], tx)
    }
  }

  object Until extends ProductReader[Until] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Until = {
      require (arity == 1 && adj == 0)
      val _stop = in.readEx[Long]()
      new Until(_stop)
    }
  }
  case class Until(stop: Ex[Long]) extends Ex[_SpanLike] {
    override def productPrefix: String = s"Span$$Until" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, _SpanLike]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new graph.UnaryOp.Expanded(UntilOp(), stop.expand[T], tx)
    }
  }

  override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Ex[_Span] = {
    require (arity == 2 && adj == 0)
    val _start  = in.readEx[Long]()
    val _stop   = in.readEx[Long]()
    Span(_start, _stop)
  }
}
