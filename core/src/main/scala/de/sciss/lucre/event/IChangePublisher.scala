/*
 *  IChangePublisher.scala
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

package de.sciss.lucre.event

import de.sciss.lucre.stm.Base
import de.sciss.model.Change

trait IChangePublisher[S <: Base[S], +A] extends IPublisher[S, Change[A]] {
  override def changed: IChangeEvent[S, A]
}