/*
 *  Form.scala
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

package de.sciss.lucre.stm

/** Any form parametrized in a `Base` system.
  * This trait allows us to pattern match against
  * heterogeneous objects whose only common feature
  * is that they share the system.
  */
trait Form[S]