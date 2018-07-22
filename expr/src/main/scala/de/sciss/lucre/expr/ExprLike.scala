/*
 *  ExprLike.scala
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

import de.sciss.lucre.event.Observable
import de.sciss.lucre.stm.{Base, Disposable}
import de.sciss.model.Change

trait ExprLike[S <: Base[S], +A] extends Disposable[S#Tx] {
  def changed: Observable[S#Tx, Change[A]]

  def value(implicit tx: S#Tx): A
}
