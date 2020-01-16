/*
 *  Trig.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2020 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr.graph

import de.sciss.lucre.event.IPush.Parents
import de.sciss.lucre.event.impl.IGenerator
import de.sciss.lucre.event.{IEvent, IPull, ITargets}
import de.sciss.lucre.expr.{Context, IAction, ITrigger, TrigOps}
import de.sciss.lucre.stm.Sys

import scala.language.{higherKinds, implicitConversions}

object Trig {
  final val Some: Option[Unit] = scala.Some(())

  implicit def ops(t: Trig): TrigOps = new TrigOps(t)

  /** Creates a "standalone" trigger that can be activated as an action.
    */
  def apply(): Act with Trig = Impl()

  private final class Expanded[S <: Sys[S]](implicit protected val targets: ITargets[S])
    extends ITrigger[S] with IAction[S] with IGenerator[S, Unit] {

    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Unit] = {
      if (pull.isOrigin(this)) Trig.Some
      else {
        val p: Parents[S] = pull.parents(this)
        if (p.exists(pull(_).isDefined)) Trig.Some else None
      }
    }

    def changed: IEvent[S, Unit] = this

    def executeAction()(implicit tx: S#Tx): Unit = fire(())

    def addSource(tr: ITrigger[S])(implicit tx: S#Tx): Unit =
      tr.changed ---> this

    def dispose()(implicit tx: S#Tx): Unit = ()
  }


  private final case class Impl() extends Act with Trig {
    override def productPrefix: String = "Trig" // serialization

    type Repr[S <: Sys[S]] = IAction[S] with ITrigger[S]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new Expanded[S]
    }
  }
}

/** A trigger element.
  *
  * ''Important:'' Implementation inadvertently run into trouble if they do not
  * extend `Trig.Lazy` to avoid "splitting" of the event paths. The only reason this
  * is not enforced is that implementation may already mixin lazy traits such as
  * `Control` (and by extension, `Widget`).
  */
trait Trig extends Lazy {
  type Repr[S <: Sys[S]] <: ITrigger[S]
}