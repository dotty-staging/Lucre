/*
 *  ConstImpl.scala
 *  (Lucre 4)
 *
 *  Copyright (c) 2009-2022 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre
package impl

import de.sciss.model.Change

trait ExprConstImpl[T <: Txn[T], A] extends Expr.Const[T, A] with ConstObjImpl[T, Change[A]] {
  final def value(implicit tx: T): A = constValue

  override def toString: String = constValue.toString
}