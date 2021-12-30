/*
 *  Plain.scala
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

import de.sciss.lucre
import de.sciss.lucre.impl.PlainImpl

object Plain {
  implicit val instance: Plain = PlainImpl()

  type Id = Ident[Plain]
}
trait Plain extends Base with Cursor[Plain] with Exec[Plain] {
  type Tx     = Plain
  type Var[A] = lucre.Var[Tx, A]
  type Id     = Plain.Id
  type I      = Plain
}
