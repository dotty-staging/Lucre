/*
 *  Identifiable.scala
 *  (Lucre)
 *
 *  Copyright (c) 2011-2015 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.stm

trait Identifiable[+ID] {
  def id: ID

  override def equals(that: Any): Boolean = that match {
    case m: Identifiable[_] =>
      id == m.id
    case _ => super.equals(that)
  }

  override def hashCode = id.hashCode()
}