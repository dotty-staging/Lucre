/*
 *  ExOptionFlatMap.scala
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

import de.sciss.lucre.expr.graph.impl.ExpandedFlatMapExOption
import de.sciss.lucre.expr.{Context, IExpr}
import de.sciss.lucre.stm.Sys

final case class ExOptionFlatMap[A, B](in: Ex[Option[A]], fun: Ex[Option[B]])
  extends Ex[Option[B]] {

  type Repr[S <: Sys[S]] = IExpr[S, Option[B]]

  protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
    val inEx = in.expand[S]
    import ctx.targets
    new ExpandedFlatMapExOption[S, A, B](inEx, fun, tx)
  }
}
