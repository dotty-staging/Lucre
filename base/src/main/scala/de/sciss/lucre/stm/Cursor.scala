/*
 *  Cursor.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2018 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre
package stm

trait Cursor[S <: Base[S]] {
  def step[A](fun: S#Tx => A): A

  def position(implicit tx: S#Tx): S#Acc
}