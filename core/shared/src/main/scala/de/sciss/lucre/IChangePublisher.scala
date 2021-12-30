/*
 *  IChangePublisher.scala
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

import de.sciss.model.Change

trait IChangePublisher[T <: Exec[T], +A] extends IPublisher[T, Change[A]] {
  override def changed: IChangeEvent[T, A]
}