/*
 *  Ops.scala
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

package de.sciss.lucre.expr

import de.sciss.lucre.{BooleanObj, DoubleObj, IntObj, LongObj, SpanLikeObj, SpanObj, StringObj, Txn}

import scala.language.implicitConversions

object Ops extends Ops
trait Ops {
  implicit def intObjOps      [T <: Txn[T]](obj: IntObj     [T]): IntExtensions     .Ops[T] = new IntExtensions     .Ops(obj)
  implicit def longObjOps     [T <: Txn[T]](obj: LongObj    [T]): LongExtensions    .Ops[T] = new LongExtensions    .Ops(obj)
  implicit def doubleObjOps   [T <: Txn[T]](obj: DoubleObj  [T]): DoubleExtensions  .Ops[T] = new DoubleExtensions  .Ops(obj)
  implicit def booleanObjOps  [T <: Txn[T]](obj: BooleanObj [T]): BooleanExtensions .Ops[T] = new BooleanExtensions .Ops(obj)
  implicit def stringObjOps   [T <: Txn[T]](obj: StringObj  [T]): StringExtensions  .Ops[T] = new StringExtensions  .Ops(obj)
  implicit def spanLikeObjOps [T <: Txn[T]](obj: SpanLikeObj[T]): SpanLikeExtensions.Ops[T] = new SpanLikeExtensions.Ops(obj)
  implicit def spanObjOps     [T <: Txn[T]](obj: SpanObj    [T]): SpanExtensions    .Ops[T] = new SpanExtensions    .Ops(obj)
}
