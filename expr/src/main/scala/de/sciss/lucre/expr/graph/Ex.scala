/*
 *  Ex.scala
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

package de.sciss.lucre.expr.graph

import java.io.File

import de.sciss.lucre.aux.Aux
import de.sciss.lucre.expr.graph.impl.{ExpandedMapActOption, ExpandedMapExOption, ExpandedMapExSeq}
import de.sciss.lucre.expr.{Context, ExBooleanOps, ExFileOps, ExOps, ExOptionOps, ExSeq, ExSeqOps, ExSpanOps, ExStringOps, ExTuple2, ExTuple2Ops, Graph, IExpr}
import de.sciss.lucre.stm.Sys
import de.sciss.serial.DataInput
import de.sciss.span.SpanLike

import scala.language.{higherKinds, implicitConversions}

object Ex {
  // ---- implicits ----

  implicit def const[A: Value](x: A): Ex[A] = Const(x)

  object Value {
    implicit object anyVal    extends Value[AnyVal  ]
    implicit object string    extends Value[String  ]
    implicit object file      extends Value[File    ]
    implicit object spanLike  extends Value[SpanLike]
    implicit object act       extends Value[Act     ]

    implicit def tuple2 [A: Value, B: Value]: Value[(A, B)] = null

    implicit def option [A: Value]: Value[Option[A]] = null
    implicit def seq    [A: Value]: Value[Seq   [A]] = null
  }
  trait Value[-A]

//  implicit def const(x: Int     ): Ex[Int     ] = Const(x)
//  implicit def const(x: Long    ): Ex[Long    ] = Const(x)
//  implicit def const(x: Double  ): Ex[Double  ] = Const(x)
//  implicit def const(x: Boolean ): Ex[Boolean ] = Const(x)
//  implicit def const(x: String  ): Ex[String  ] = Const(x)
//  implicit def const(x: File    ): Ex[File    ] = Const(x)
//
//  implicit def const[A <: SpanLike](x: A): Ex[A] = Const(x)

//  implicit def liftTuple[A: Value, B: Value](x: (A, B)): Ex[(A, B)] = Const(x)

  implicit def liftTuple2_1[A, B: Value](x: (Ex[A], B)): Ex[(A, B)] = ExTuple2(x._1, Const(x._2))
  implicit def liftTuple2_2[A: Value, B](x: (A, Ex[B])): Ex[(A, B)] = ExTuple2(Const(x._1), x._2)

//  implicit def liftTupleL[A, B: Value](x: (Ex[A], B)): Ex[(A, B)] = ...
//  implicit def liftTupleR[A: Value, B](x: (A, Ex[B])): Ex[(A, B)] = ...

//  implicit def liftOption[A: Value](x: Option[A]): Ex[Option[A]] = Const(x)

  implicit def liftOptionEx[A](x: Option[Ex[A]]): Ex[Option[A]] = x match {
    case Some(ex) => UnaryOp(UnaryOp.OptionSome[A](), ex)
    case None     => Const(Option.empty[A])
  }

//  implicit def liftSeq[A: Value](x: Seq[A]): Ex[Seq[A]] = Const(x) // immutable(x))

  implicit def liftSeqEx[A](x: Seq[Ex[A]]): Ex[Seq[A]] =
    if (x.isEmpty) Const(Nil) else ExSeq(x: _*) // immutable(x): _*)

  implicit def ops      [A]   (x: Ex[A])        : ExOps       [A]     = new ExOps       (x)
  implicit def seqOps   [A]   (x: Ex[Seq  [A]]) : ExSeqOps    [A]     = new ExSeqOps    (x)
  implicit def optionOps[A]   (x: Ex[Option[A]]): ExOptionOps [A]     = new ExOptionOps (x)
  implicit def tuple2Ops[A, B](x: Ex[(A, B)])   : ExTuple2Ops [A, B]  = new ExTuple2Ops (x)
  implicit def booleanOps     (x: Ex[Boolean])  : ExBooleanOps        = new ExBooleanOps(x)
  implicit def stringOps      (x: Ex[String])   : ExStringOps         = new ExStringOps (x)
  implicit def spanOps[A <: SpanLike](x: Ex[A]) : ExSpanOps   [A]     = new ExSpanOps   (x)
  implicit def fileOps        (x: Ex[File])     : ExFileOps           = new ExFileOps   (x)

  //////////////////////////////

  private val anyCanMapExOption     = new CanMapExOption    [Any]
  private val anyCanMapExSeq        = new CanMapExSeq       [Any]

  private val anyCanFlatMapExOption = new CanFlatMapExOption[Any]

  private lazy val _init: Unit = {
    Aux.addFactory(anyCanMapExOption    )
    Aux.addFactory(anyCanMapExSeq       )
    Aux.addFactory(CanMapActOption      )
    Aux.addFactory(anyCanFlatMapExOption)
  }

  def init(): Unit = _init

  final case class MapExOption[A, B] private (in: Ex[Option[A]], fun: Ex[B])
    extends Ex[Option[B]] {

    type Repr[S <: Sys[S]] = IExpr[S, Option[B]]

    override def productPrefix: String = s"Ex$$MapExOption" // serialization

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      val inEx = in.expand[S]
      import ctx.targets
//      new ExpandedMapExOption[S, A, B](inEx, fun, tx)
      new ExpandedMapExOption[S, A, B](inEx, fun, tx)
    }
  }

  final case class MapExSeq[A, B](in: Ex[Seq[A]], it: It[A], closure: Graph, fun: Ex[B]) extends Ex[Seq[B]] {
    type Repr[S <: Sys[S]] = IExpr[S, Seq[B]]

    override def productPrefix: String = s"Ex$$MapExSeq" // serialization

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      val inEx = in.expand[S]
      val itEx = it.expand[S]
      import ctx.targets
      new ExpandedMapExSeq[S, A, B](inEx, itEx, /*closure, */ fun, tx)
    }
  }

  final case class MapActOption[A] private (in: Ex[Option[A]], fun: Act)
    extends Ex[Option[Act]] {

    type Repr[S <: Sys[S]] = IExpr[S, Option[Act]]

    override def productPrefix: String = s"Ex$$MapActOption" // serialization

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      val inEx = in.expand[S]
      import ctx.targets
      new ExpandedMapActOption[S, A](inEx, fun, tx)
    }
  }

  object CanMap {
    implicit def exOption[B]: CanMap[Option, Ex[B], Ex[Option[B]]]    = anyCanMapExOption .asInstanceOf[CanMapExOption  [B]]
    implicit def exSeq   [B]: CanMap[Seq   , Ex[B], Ex[Seq   [B]]]    = anyCanMapExSeq    .asInstanceOf[CanMapExSeq     [B]]
    implicit def actOption  : CanMap[Option, Act  , Ex[Option[Act]]]  = CanMapActOption
    // implicit def actSeq     : CanMap[Seq   , Act  , Ex[Seq   [Act]]]  = ...
  }
  trait CanMap[-From[_], -B, +To] extends Aux {
    def map[A](from: Ex[From[A]], fun: Ex[A] => B): To
  }

  object CanFlatMap {
    implicit def exOption [B]: CanFlatMap[Option, Ex[Option[B]], Ex[Option[B]]]  = anyCanFlatMapExOption.asInstanceOf[CanFlatMapExOption[B]]
    // implicit def exSeq    [B]: CanFlatMap[Seq   , Ex[Seq   [B]], Ex[Seq   [B]]]  = ...
    // implicit def exSeqOpt [B]: CanFlatMap[Seq   , Ex[Option[B]], Ex[Seq   [B]]]  = ...
  }
  trait CanFlatMap[-From[_], -B, +To] {
    def flatMap[A](from: Ex[From[A]], fun: Ex[A] => B): To
  }

  // --------------------- impl ---------------------

  // common to all type classes
  private abstract class MapSupport extends Aux with Aux.Factory {
    def mkClosure[A, B](fun: Ex[A] => B): (It[A], Graph, B) = {
      val b     = Graph.builder
      val it    = b.allocToken[A]()
      val (c, r) = Graph.withResult {
        fun(it)
      }
      (it, c, r)
    }

    def readIdentifiedAux(in: DataInput): Aux = this
  }

  private final class CanMapExOption[B] extends MapSupport
    with CanMap[Option, Ex[B], Ex[Option[B]]] {

    final val id = 1001

    override def toString = "CanMapExOption"

    def map[A](from: Ex[Option[A]], fun: Ex[A] => Ex[B]): Ex[Option[B]] = {
      val fOut = fun(OptionGet(from))
      MapExOption[A, B](in = from, fun = fOut)
    }
  }

  private final class CanMapExSeq[B] extends MapSupport
    with CanMap[Seq, Ex[B], Ex[Seq[B]]] {

    final val id = 1002

    override def toString = "CanMapExSeq"

    def map[A](from: Ex[Seq[A]], fun: Ex[A] => Ex[B]): Ex[Seq[B]] = {
      val (it, closure, res) = mkClosure(fun)
      MapExSeq[A, B](in = from, it = it, closure = closure, fun = res)
    }
  }

  private final object CanMapActOption extends MapSupport
    with CanMap[Option, Act, Ex[Option[Act]]] {

    final val id = 1003

    override def toString = "CanMapActOption"

    def map[A](from: Ex[Option[A]], fun: Ex[A] => Act): Ex[Option[Act]] = {
      val fOut = fun(OptionGet(from))
      MapActOption[A](in = from, fun = fOut)
    }
  }

  private final class CanFlatMapExOption[B] extends MapSupport
    with CanFlatMap[Option, Ex[Option[B]], Ex[Option[B]]] {

    final val id = 1004

    override def toString = "CanFlatMapExOption"

    def flatMap[A](from: Ex[Option[A]], fun: Ex[A] => Ex[Option[B]]): Ex[Option[B]] = {
      val fOut = fun(OptionGet(from))
      ExOptionFlatMap(in = from, fun = fOut)
    }
  }
}
trait Ex[+A] extends Lazy {
  type Repr[S <: Sys[S]] <: IExpr[S, A]
}
