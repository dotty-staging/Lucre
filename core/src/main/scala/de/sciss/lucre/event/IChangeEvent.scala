/*
 *  IChangeEvent.scala
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

trait IChangeEvent[S <: Base[S], +A] extends IEvent[S, Change[A]] {
  private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Change[A]] = {
    val before  = pullChange(pull, isNow = false)
    val now     = pullChange(pull, isNow = true )
    if (now == before) None else Some(Change(before = before, now = now))
  }

  private[lucre] def pullChange(pull: IPull[S], isNow: Boolean)(implicit tx: S#Tx): A
}