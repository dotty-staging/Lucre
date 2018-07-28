/*
 *  ExAttrBridge.scala
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

import de.sciss.lucre.aux
import de.sciss.lucre.expr.impl.{ExAttrBridgeImpl => Impl}
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.span.{Span, SpanLike}

import scala.collection.immutable.{IndexedSeq => Vec}

object ExAttrBridge {
  implicit val int      : ExAttrBridge[Int        ] = new Impl(IntObj       )
  implicit val long     : ExAttrBridge[Long       ] = new Impl(LongObj      )
  implicit val double   : ExAttrBridge[Double     ] = new Impl(DoubleObj    )
  implicit val boolean  : ExAttrBridge[Boolean    ] = new Impl(BooleanObj   )
  implicit val string   : ExAttrBridge[String     ] = new Impl(StringObj    )
  implicit val spanLike : ExAttrBridge[SpanLike   ] = new Impl(SpanLikeObj  )
  implicit val span     : ExAttrBridge[Span       ] = new Impl(SpanObj      )
  implicit val intVec   : ExAttrBridge[Vec[Int   ]] = new Impl(IntVector    )
  implicit val doubleVec: ExAttrBridge[Vec[Double]] = new Impl(DoubleVector )

}
trait ExAttrBridge[A] extends aux.Aux {
  // def peer: Obj.Type

  def cellView[S <: Sys[S]](obj: Obj[S], key: String)(implicit tx: S#Tx): CellView.Var[S, Option[A]]
}
