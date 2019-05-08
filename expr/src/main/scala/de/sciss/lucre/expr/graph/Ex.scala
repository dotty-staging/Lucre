/*
 *  Ex.scala
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

import de.sciss.lucre.expr.IExpr
import de.sciss.lucre.stm.Sys

import scala.language.higherKinds

trait Ex[+A] extends Lazy {
  type Repr[S <: Sys[S]] <: IExpr[S, A]
}
