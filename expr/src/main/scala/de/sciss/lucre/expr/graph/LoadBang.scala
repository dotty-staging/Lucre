/*
 *  LoadBang.scala
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

import de.sciss.lucre.event.impl.IGenerator
import de.sciss.lucre.event.{IEvent, IPull, ITargets}
import de.sciss.lucre.expr.{Context, IControl, ITrigger}
import de.sciss.lucre.stm.{Base, Sys}

object LoadBang {
  private final class Expanded[S <: Base[S]](tx0: S#Tx)
                                            (implicit protected val targets: ITargets[S])
    extends IControl[S] with ITrigger[S] with IGenerator[S, Unit] {

    override def toString: String = "LoadBang()"

    def changed: IEvent[S, Unit] = this

    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Unit] =
      Trig.Some

    def dispose()(implicit tx: S#Tx): Unit = ()

    def initControl()(implicit tx: S#Tx): Unit = fire(())
  }

}
final case class LoadBang() extends Control with Trig {
  type Repr[S <: Base[S]] = IControl[S] with ITrigger[S]

  protected def mkControl[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
    import ctx.targets
    new LoadBang.Expanded[S](tx)
  }
}
