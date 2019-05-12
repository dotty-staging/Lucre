/*
 *  TimeStamp.scala
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

package de.sciss.lucre.expr.graph

import java.util.Locale

import de.sciss.lucre.event.IPush.Parents
import de.sciss.lucre.event.impl.{IEventImpl, IGenerator}
import de.sciss.lucre.event.{Caching, IEvent, IPull, ITargets}
import de.sciss.lucre.expr.{Context, IAction, IExpr, ITrigger, graph}
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.model.Change

import scala.concurrent.stm.Ref

object TimeStamp {
  final case class Update(ts: TimeStamp) extends Act {
    override def productPrefix: String = s"TimeStamp$$Update" // serialization

    type Repr[S <: Sys[S]] = IAction[S]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new ExpandedUpdate[S](ts.expand[S], tx)
    }
  }

//  final case class Format__(ts: TimeStamp, s: Ex[String]) extends Ex[String] {
//    override def productPrefix: String = s"TimeStamp$$Format" // serialization
//
//    type Repr[S <: Sys[S]] = IExpr[S, String]
//
//    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = ...
//  }

  final case class Format[S <: Sys[S]]() extends BinaryOp.Op[Long, String, String] {
    override def productPrefix = s"TimeStamp$$Format" // serialization

    def apply(a: Long, b: String): String =
      try {
        val f = new java.text.SimpleDateFormat(b, Locale.US)
        f.format(new java.util.Date(a))
      } catch {
        case _: IllegalArgumentException =>
          s"Invalid format '$b'"
      }
  }

//  trait Expanded[S <: Sys[S]] extends IExpr[S, Long] {
//    def update(epochMillis: Long)(implicit tx: S#Tx): Unit
//  }

  // ---- impl ----

  // XXX TODO --- perhaps use this as a general relay building block
  private final class ExpandedUpdate[S <: Sys[S]](ts: IExpr[S, Long], tx0: S#Tx)
                                                 (implicit protected val targets: ITargets[S])
    extends IAction[S] with IGenerator[S, Unit] {

    this.--->(ts.changed)(tx0)

    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Unit] = {
      if (pull.isOrigin(this)) Trig.Some
      else {
        val p: Parents[S] = pull.parents(this)
        if (p.exists(pull(_).isDefined)) Trig.Some else None
      }
    }

    def executeAction()(implicit tx: S#Tx): Unit = fire(())

    def addSource(tr: ITrigger[S])(implicit tx: S#Tx): Unit =
      tr.changed ---> this

    def dispose()(implicit tx: S#Tx): Unit =
      this.-/->(ts.changed)
  }

  private final class Expanded[S <: Sys[S]](implicit protected val targets: ITargets[S])
    extends IExpr[S, Long] with IEventImpl[S, Change[Long]] with Caching {

    // should we use universe.scheduler?
    private[this] val ref = Ref(System.currentTimeMillis())

    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx) : Option[Change[Long]] = {
      val p: Parents[S] = pull.parents(this)
      if (p.exists(pull(_).isDefined)) {
        val now     = System.currentTimeMillis()
        val before  = ref.swap(now)
        if (before != now) Some(Change(before, now)) else None
      } else None
    }

    def value(implicit tx: S#Tx): Long = ref()

    def changed: IEvent[S, Change[Long]] = this

    def dispose()(implicit tx: S#Tx): Unit = ()
  }
}
final case class TimeStamp() extends Ex[Long] {
  type Repr[S <: Sys[S]] = IExpr[S, Long] // TimeStamp.Expanded[S]

  /** Creates a string representation based on `java.text.SimpleDateFormat`, US locale, and
    * default (system) time-zone.
    */
  def format(s: Ex[String]): Ex[String] =
    BinaryOp(TimeStamp.Format(), this, s)

  /** Trigger this to update the time stamp to the current time. */
  def update: Act = TimeStamp.Update(this)

  protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
    import ctx.targets
    new graph.TimeStamp.Expanded[S]
  }
}