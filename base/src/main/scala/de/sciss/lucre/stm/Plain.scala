/*
 *  Plain.scala
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

package de.sciss.lucre.stm

import de.sciss.lucre.stm

object Plain {
//  def apply(): Plain = impl.PlainImpl()
  implicit val instance: Plain = impl.PlainImpl()
}
trait Plain extends Base[Plain] with Cursor[Plain] with Executor[Plain] {
  type Tx     = Plain
  type Acc    = Unit
  type Var[A] = stm.Var[Tx, A]
  type Id     = stm.Identifier[Plain]
}
