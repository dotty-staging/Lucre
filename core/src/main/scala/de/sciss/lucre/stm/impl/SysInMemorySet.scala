/*
 *  SysInMemorySet.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2019 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.stm
package impl

import scala.concurrent.stm.TSet

final class SysInMemorySet[S <: Sys[S], A] extends RefSet[S, A] {
  private[this] val peer = TSet.empty[A]

  def add     (elem: A)(implicit tx: S#Tx): Boolean = peer.add     (elem)(tx.peer)
  def remove  (elem: A)(implicit tx: S#Tx): Boolean = peer.remove  (elem)(tx.peer)
  def contains(elem: A)(implicit tx: S#Tx): Boolean = peer.contains(elem)(tx.peer)
  def size             (implicit tx: S#Tx): Int     = peer.size          (tx.peer)
  def isEmpty          (implicit tx: S#Tx): Boolean = peer.isEmpty       (tx.peer)

  def foreach[B](f: A => B)(implicit tx: S#Tx): Unit = peer.foreach(f)(tx.peer)

  def toSet(implicit tx: S#Tx): Set[A] = peer.snapshot
}
