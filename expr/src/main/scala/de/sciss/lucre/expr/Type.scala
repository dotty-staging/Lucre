/*
 *  Type.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2020 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr

import de.sciss.lucre.event.Targets
import de.sciss.lucre.expr.impl.{ExObjBridgeImpl, ExSeqObjBridgeImpl}
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.lucre.{adjunct, expr, stm}
import de.sciss.serial.{DataInput, ImmutableSerializer, Serializer}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.language.{higherKinds, implicitConversions}

object Type {
  trait Extension {
    def name: String

    /** Lowest id of handled operators */
    val opLo : Int
    /** Highest id of handled operators. Note: This value is _inclusive_ */
    val opHi : Int

    override def toString = s"$name [lo = $opLo, hi = $opHi]"
  }

  trait Extension1[+Repr[~ <: Sys[~]]] extends Extension {
    def readExtension[S <: Sys[S]](opId: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                                  (implicit tx: S#Tx): Repr[S]
  }

  trait Extension2[+Repr[~ <: Sys[~], _]] extends Extension {
    def readExtension[S <: Sys[S], T1](opId: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                                      (implicit tx: S#Tx): Repr[S, T1]
  }

  trait Extension3[+Repr[~ <: Sys[~], _, _]] extends Extension {
    def readExtension[S <: Sys[S], T1, T2](opId: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                                          (implicit tx: S#Tx): Repr[S, T1, T2]
  }

  trait _1[Repr[~ <: Sys[~]]] extends Obj.Type {
//    implicit def serializer[S <: Sys[S]]: evt.Serializer[S, Repr[S]]
//
//    def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Repr[S]

    /** This method is not thread-safe. We assume extensions are registered upon application start only! */
    def registerExtension(ext: Type.Extension1[Repr]): Unit
  }

  private lazy val _init: Unit = {
    adjunct.Adjunct.addFactory(Type.ObjBridge)
    adjunct.Adjunct.addFactory(Type.SeqObjBridge)
  }

  def init(): Unit = _init

  object ObjBridge extends adjunct.Adjunct.Factory {
    final val id = 1000

    def readIdentifiedAdjunct(in: DataInput): adjunct.Adjunct = {
      val typeId  = in.readInt()
      val peer    = Obj.getType(typeId)
      new ExObjBridgeImpl(peer.asInstanceOf[Type.Expr[Any, ({ type R[~ <: Sys[~]] <: expr.Expr[~, Any] }) # R]])
    }
  }

  object SeqObjBridge extends adjunct.Adjunct.Factory {
    final val id = 1010

    def readIdentifiedAdjunct(in: DataInput): adjunct.Adjunct = {
      val typeId  = in.readInt()
      val peer    = Obj.getType(typeId)
      new ExSeqObjBridgeImpl(peer.asInstanceOf[Type.Expr[Vec[Any], ({ type R[~ <: Sys[~]] <: expr.Expr[~, Vec[Any]] }) # R]])
    }
  }

  trait Expr[A1, Repr[~ <: Sys[~]] <: expr.Expr[~, A1]] extends Obj.Type {
    type A = A1
    type _Ex   [S <: Sys[S]] = Repr[S] // yeah, well, we're waiting for Dotty
    // N.B.: this causes trouble:
//     type Var  [S <: Sys[S]] = Repr[S] with expr.Expr.Var  [S, A, _Ex]
    type Var  [S <: Sys[S]] = Repr[S] with stm.Var[S#Tx, Repr[S]]
    type Const[S <: Sys[S]] = Repr[S] with expr.Expr.Const[S, A]

    // ---- abstract ----

    def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Repr[S]

    implicit def serializer   [S <: Sys[S]]: Serializer[S#Tx, S#Acc, Repr[S]]
    implicit def varSerializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Var [S]]

    implicit def valueSerializer: ImmutableSerializer[A]

    // ---- public ----

    object Var {
      def unapply[S <: Sys[S]](expr: _Ex[S]): Option[Var[S]] = {
        // !!! this wrongly reports `true` for `Const`, probably due
        // to some erasure that scalac doesn't warn about
        // if (expr.isInstanceOf[Var[_]]) Some(expr.asInstanceOf[Var[S]]) else None

        if (expr.isInstanceOf[stm.Var[_, _]]) Some(expr.asInstanceOf[Var[S]]) else None
      }
    }

    implicit def newConst [S <: Sys[S]](value: A     )(implicit tx: S#Tx): Const[S]
    def newVar            [S <: Sys[S]](init: Repr[S])(implicit tx: S#Tx): Var  [S]

    def readConst[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Const[S]
    def readVar  [S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Var  [S]

    def tryParse(value: Any): Option[A]
  }
}
