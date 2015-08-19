package de.sciss.lucre.expr
package impl

import de.sciss.lucre
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.{event => evt}
import de.sciss.model.Change
import de.sciss.serial.DataOutput

trait Tuple1Op[A, T1] /* extends TupleOp */ {
  def id: Int

  def value(a: T1): A

  final def unapply[S <: Sys[S]](ex: Expr[S, A])(implicit tx: S#Tx): Option[Expr[S, T1]] = ex match {
    case tup: Tuple1[_, _, _] if tup.op == this => Some(tup._1.asInstanceOf[Expr[S, T1]])
    case _ => None
  }

  def toString[S <: Sys[S]](_1: Expr[S, T1]): String
}

final class Tuple1[S <: Sys[S], A, T1](a: Type.Expr[A], typeID: Int, val op: Tuple1Op[A, T1],
                                       protected val targets: evt.Targets[S],
                                       val _1: Expr[S, T1])
  extends impl.NodeImpl[S, A] {

  protected def reader = a.serializer[S]

  def connect()(implicit tx: S#Tx): this.type = {
    _1.changed ---> changed
    this
  }

  private[this] def disconnect()(implicit tx: S#Tx): Unit = _1.changed -/-> changed

  protected def disposeData()(implicit tx: S#Tx): Unit = disconnect()

  def value(implicit tx: S#Tx) = op.value(_1.value)

  protected def writeData(out: DataOutput): Unit = {
    out.writeByte(1)
    out.writeInt(typeID)
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

  override def toString = op.toString(_1)
}

trait Tuple2Op[A, T1, T2] /* extends TupleOp */ {
  def id: Int

  def value(a: T1, b: T2): A

  final protected def writeTypes(out: DataOutput) = ()

  final def unapply[S <: Sys[S]](ex: Expr[S, A])(implicit tx: S#Tx): Option[(Expr[S, T1], Expr[S, T2])] =
    ex match {
      case tup: Tuple2[_, _, _, _] if tup.op == this =>
        Some((tup._1.asInstanceOf[Expr[S, T1]], tup._2.asInstanceOf[Expr[S, T2]]))
      case _ => None
    }

  def toString[S <: Sys[S]](_1: Expr[S, T1], _2: Expr[S, T2]): String
}

final class Tuple2[S <: Sys[S], A, T1, T2](a: Type.Expr[A], typeID: Int, val op: Tuple2Op[A, T1, T2],
                                           protected val targets: evt.Targets[S],
                                           val _1: Expr[S, T1], val _2: Expr[S, T2])
  extends impl.NodeImpl[S, A] {

  protected def reader = a.serializer[S]

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

  def value(implicit tx: S#Tx) = op.value(_1.value, _2.value)

  protected def writeData(out: DataOutput): Unit = {
    out.writeByte(2)
    out.writeInt(typeID)
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

  override def toString = op.toString(_1, _2)
}