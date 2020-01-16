/*
 *  ITrigger.scala
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

package de.sciss.lucre.expr

import de.sciss.lucre.event.IPublisher
import de.sciss.lucre.stm.{Base, Disposable}

trait ITrigger[S <: Base[S]] extends IPublisher[S, Unit] with Disposable[S#Tx]
