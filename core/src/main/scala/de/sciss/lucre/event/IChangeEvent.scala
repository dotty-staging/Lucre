/*
 *  IChangeEvent.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2020 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.event

import de.sciss.lucre.event.IPull.Phase
import de.sciss.lucre.stm.Base
import de.sciss.model.Change

trait IChangeEvent[S <: Base[S], +A] extends IEvent[S, Change[A]] {
  private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Change[A]] = {
    val before  = pullChange(pull)(tx, IPull.Before)
    val now     = pullChange(pull)(tx, IPull.Now   )
    if (now == before) None else Some(Change(before = before, now = now))
  }

  private[lucre] def pullChange(pull: IPull[S])(implicit tx: S#Tx, phase: Phase): A
}