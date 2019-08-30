/*
 *  PartialMapHandler.scala
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

import de.sciss.serial.{ImmutableSerializer, DataInput}

trait PartialMapHandler[S <: Sys[S]] {
  def getIndexTreeTerm(term: Long)(implicit tx: S#Tx): Long

  def readPartialMap[A](in: DataInput)
                       (implicit tx: S#Tx, serializer: ImmutableSerializer[A]): IndexMap[S, A]

  def newPartialMap[A](rootValue: A)
                      (implicit tx: S#Tx, serializer: ImmutableSerializer[A]): IndexMap[S, A]
}