/*
 *  Ex.scala
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

package de.sciss.lucre.expr

import de.sciss.lucre.aux.ProductWithAux
import de.sciss.lucre.stm.Base

trait Ex[A] extends ProductWithAux {
  def value[S <: Base[S]](implicit /* ctx: Context[S], */ tx: S#Tx): A
}
