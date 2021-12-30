/*
 *  Identifier.scala
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

package de.sciss.lucre.confluent

import de.sciss.lucre.{Ident => LIdent}

trait Ident[T <: Txn[T]] extends LIdent[T] {
  def base: Int
  def path: Access[T]
}
