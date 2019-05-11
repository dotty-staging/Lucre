/*
 *  OptionGet.scala
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

import de.sciss.lucre.event.{IPull, ITargets}
import de.sciss.lucre.expr.graph.impl.MappedIExpr
import de.sciss.lucre.expr.{Context, IExpr}
import de.sciss.lucre.stm.Sys
import de.sciss.model.Change

object OptionGet {
  private final class Expanded[S <: Sys[S], A](in: IExpr[S, Option[A]], tx0: S#Tx)(implicit targets: ITargets[S])
    extends MappedIExpr[S, Option[A], A](in, tx0) {

    protected def mapValue(inValue: Option[A]): A = inValue.get

    override private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Change[A]] =
      pull(in.changed).flatMap { ch =>
        // simply don't fire if the option was not or is not defined
        if (ch.before.isEmpty || ch.now.isEmpty) None else {
          val before  = mapValue(ch.before )
          val now     = mapValue(ch.now    )
          if (before == now) None else Some(Change(before, now))
        }
      }
  }
}
case class OptionGet[A](in: Ex[Option[A]]) extends Ex[A] {
  type Repr[S <: Sys[S]] = IExpr[S, A]

  protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
    import ctx.targets
    new OptionGet.Expanded(in.expand[S], tx)
  }
}
