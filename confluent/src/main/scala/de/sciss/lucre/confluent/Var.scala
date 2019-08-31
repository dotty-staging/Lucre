/*
 *  Var.scala
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

package de.sciss.lucre.confluent

import de.sciss.lucre.stm

trait Var[S <: Sys[S], A] extends stm.Var[S#Tx, A] with Source[S, A] {
  private[confluent] def setInit(value: A)(implicit tx: S#Tx): Unit
}