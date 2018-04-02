/*
 *  Tuples.scala
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
package impl

import de.sciss.lucre.stm.Sys
import de.sciss.lucre.{event => evt}
import de.sciss.model.Change
import de.sciss.serial.DataOutput

import scala.language.higherKinds

trait Tuple1Op[A, T1, ReprA[~ <: Sys[~]] <: Expr[~, A], ReprT1[~ <: Sys[~]] <: Expr[~, T1]] {
  def id: Int

  def value(a: T1): A

  final def unapply[S <: Sys[S]](ex: ReprA[S])(implicit tx: S#Tx): Option[ReprT1[S]] = ex match {
    case tup0: Tuple1[_, _, _, _, _] if tup0.op == this =>
      val tup = tup0.asInstanceOf[Tuple1[S, A, T1, ReprA, ReprT1]]
      Some(tup._1)
    case _ => None
  }

  def toString[S <: Sys[S]](_1: ReprT1[S]): String
}

trait Tuple1[S <: Sys[S], A, T1, ReprA[~ <: Sys[~]] <: Expr[~, A], ReprT1[~ <: Sys[~]] <: Expr[~, T1]]
  extends impl.NodeImpl[S, A] {
  _: ReprA[S] =>

  def op: Tuple1Op[A, T1, ReprA, ReprT1]
  def _1: ReprT1[S]

  def connect()(implicit tx: S#Tx): this.type = {
    _1.changed ---> changed
    this
  }

  private[this] def disconnect()(implicit tx: S#Tx): Unit = _1.changed -/-> changed

  protected def disposeData()(implicit tx: S#Tx): Unit = disconnect()

  def value(implicit tx: S#Tx): A = op.value(_1.value)

  protected def writeData(out: DataOutput): Unit = {
    out.writeByte(1)  // 'node not var'
    // out.writeInt(typeID)
    out.writeInt(op.id)
    _1.write(out)
  }

  object changed extends Changed {
    private[lucre] def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Change[A]] =
      pull(_1.changed).flatMap { ach =>
        val before  = op.value(ach.before)
        val now     = op.value(ach.now   )
        if (before == now) None else Some(Change(before, now))
      }
  }

  override def toString: String = op.toString(_1)
}

trait Tuple2Op[A, T1, T2, ReprA[~ <: Sys[~]] <: Expr[~, A], ReprT1[~ <: Sys[~]] <: Expr[~, T1],
                                                            ReprT2[~ <: Sys[~]] <: Expr[~, T2]] {
  def id: Int

  def value(a: T1, b: T2): A

  // final protected def writeTypes(out: DataOutput) = ()

  final def unapply[S <: Sys[S]](ex: ReprA[S])(implicit tx: S#Tx): Option[(ReprT1[S], ReprT2[S])] =
    ex match {
      case tup0: Tuple2[_, _, _, _, _, _, _] if tup0.op == this =>
        val tup = tup0.asInstanceOf[Tuple2[S, A, T1, T2, ReprA, ReprT1, ReprT2]]
        Some((tup._1, tup._2))
      case _ => None
    }

  def toString[S <: Sys[S]](_1: ReprT1[S], _2: ReprT2[S]): String
}

trait Tuple2[S <: Sys[S], A, T1, T2, ReprA[~ <: Sys[~]] <: Expr[~, A], ReprT1[~ <: Sys[~]] <: Expr[~, T1],
  ReprT2[~ <: Sys[~]] <: Expr[~, T2]]
  extends impl.NodeImpl[S, A] {
  _: ReprA[S] =>

  def op: Tuple2Op[A, T1, T2, ReprA, ReprT1, ReprT2]
  def _1: ReprT1[S]
  def _2: ReprT2[S]

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

  def value(implicit tx: S#Tx): A = op.value(_1.value, _2.value)

  protected def writeData(out: DataOutput): Unit = {
    out.writeByte(1)  // 'node not var'
//    out.writeByte(2)
//    out.writeInt(typeID)
    out.writeInt(op.id)
    _1.write(out)
    _2.write(out)
  }

  object changed extends Changed {
    private[lucre] def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Change[A]] = {
      val _1c = _1.changed
      val _2c = _2.changed

      val _1ch = if (pull.contains(_1c)) pull(_1c) else None
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