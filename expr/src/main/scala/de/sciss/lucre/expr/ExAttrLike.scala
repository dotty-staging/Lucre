/*
 *  ExAttrLike.scala
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

trait ExAttrLike[A] extends ProductWithAux {
  def key: String

//  def <--- (that: Ex.Var[A]): Unit = that ---> this
//  def ---> (that: Ex.Var[A]): Unit = that <--- this
//  def <--> (that: Ex.Var[A]): Unit = that <--> this
}
