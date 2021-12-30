/*
 *  PlainIdentMap.scala
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
package impl

import de.sciss.lucre.Plain.Id

import scala.collection.mutable

final class PlainIdentMap[A] extends IdentMap[Plain, A] {
  private[this] val peer = mutable.Map.empty[Id, A]

  def put(id: Id, value: A)(implicit tx: Plain): Unit = peer.put(id, value)

  def get(id: Id)(implicit tx: Plain): Option[A] = peer.get(id)

  def getOrElse(id: Id, default: => A)(implicit tx: Plain): A = peer.getOrElse(id, default)

  def contains(id: Id)(implicit tx: Plain): Boolean = peer.contains(id)

  def remove(id: Id)(implicit tx: Plain): Unit = peer.remove(id)

  def dispose()(implicit tx: Plain): Unit = peer.clear()
}
