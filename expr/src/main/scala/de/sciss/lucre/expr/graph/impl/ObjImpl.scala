/*
 *  ObjImpl.scala
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

package de.sciss.lucre.expr.graph.impl

import de.sciss.lucre.expr.graph.Obj
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys

import scala.language.higherKinds

abstract class ObjImplBase[In <: Sys[In], Repr[~ <: Sys[~]] <: stm.Obj[~]](in: stm.Source[In#Tx, Repr[In]], system: In)
  extends Obj {

  type Peer[~ <: Sys[~]] = Repr[~]

  private[graph] def peer[S <: Sys[S]](implicit tx: S#Tx): Option[Repr[S]] = {
    require (tx.system == system)
    val out = in.asInstanceOf[stm.Source[S#Tx, Repr[S]]]
    Some(out())
  }
}

final class ObjImpl[In <: Sys[In]](in: stm.Source[In#Tx, stm.Obj[In]], system: In)
  extends ObjImplBase[In, stm.Obj](in, system)