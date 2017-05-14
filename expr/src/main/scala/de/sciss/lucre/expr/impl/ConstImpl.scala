/*
 *  ConstImpl.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2016 Hanns Holger Rutz. All rights reserved.
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

trait ConstImpl[S <: Sys[S], A] extends Expr.Const[S, A] with evt.impl.ConstObjImpl[S, Change[A]] {
  final def value(implicit tx: S#Tx): A = constValue

  override def toString: String = constValue.toString
}