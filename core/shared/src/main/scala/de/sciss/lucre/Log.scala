/*
 *  Log.scala
 *  (Lucre 4)
 *
 *  Copyright (c) 2009-2021 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre

import de.sciss.log.Logger

object Log {
  final val txn       : Logger = new Logger("Lucre stm")
  final val event     : Logger = new Logger("Lucre evt")
  final val confluent : Logger = new Logger("Lucre cfl")
  final val swing     : Logger = new Logger("Lucre swg")
  final val synth     : Logger = new Logger("Lucre syn")
}
