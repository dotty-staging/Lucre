/*
 *  IEvent.scala
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

package de.sciss.lucre.expr

import de.sciss.lucre.event.IChangePublisher
import de.sciss.lucre.stm.{Base, Disposable, Ref}

object IExpr {
  trait Var[S <: Base[S], A] extends IExpr[S, A] with Ref[S#Tx, IExpr[S, A]]
}
trait IExpr[S <: Base[S], +A] extends ExprLike[S, A] with IChangePublisher[S, A] with Disposable[S#Tx] {
  def value(implicit tx: S#Tx): A
}