/*
 *  Ex.scala
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

import de.sciss.lucre.aux.ProductWithAux
import de.sciss.lucre.expr.Ex.Context
import de.sciss.lucre.stm.{Base, Sys}

object Ex {
  trait Context[S <: Base[S]]
}
trait Ex[A] extends ProductWithAux {
  final def value[S <: Base[S]](implicit /* ctx: Context[S], */ tx: S#Tx): A = ???

  def expand[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): ExprLike[S, A]
}
