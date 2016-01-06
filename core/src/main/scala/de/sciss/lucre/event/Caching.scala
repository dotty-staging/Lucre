/*
 *  Caching.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2016 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.event

/** A marker trait for events that maintain a cache. Events mixing in this trait are
  * guaranteed to participate in the `pullUpdate` phase, even if no live observer is
  * attached to them.
  */
trait Caching
