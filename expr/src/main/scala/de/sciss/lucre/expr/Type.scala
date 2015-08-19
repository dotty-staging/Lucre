package de.sciss.lucre.expr

import de.sciss.lucre.event.{Targets, Node}
import de.sciss.lucre.expr
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.serial.{Serializer, DataOutput, DataInput}

import scala.language.higherKinds

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
    def readExtension[S <: Sys[S]](opID: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                                  (implicit tx: S#Tx): Repr[S] with Node[S]
  }

  trait Extension2[+Repr[~ <: Sys[~], _]] extends Extension {
    def readExtension[S <: Sys[S], T1](opID: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                                      (implicit tx: S#Tx): Repr[S, T1] with Node[S]
  }

  trait Extension3[+Repr[~ <: Sys[~], _, _]] extends Extension {
    def readExtension[S <: Sys[S], T1, T2](opID: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                                          (implicit tx: S#Tx): Repr[S, T1, T2] with Node[S]
  }

  trait _1[Repr[~ <: Sys[~]]] extends Obj.Type {
//    implicit def serializer[S <: Sys[S]]: evt.Serializer[S, Repr[S]]
//
//    def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Repr[S]

    /** This method is not thread-safe. We assume extensions are registered upon application start only! */
    def registerExtension(ext: Type.Extension1[Repr]): Unit
  }

  trait Expr[A] extends Obj.Type {

    // ---- abstract ----

    def readValue (          in : DataInput ): A
    def writeValue(value: A, out: DataOutput): Unit

    def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): expr.Expr[S, A]

    implicit def serializer   [S <: Sys[S]]: Serializer[S#Tx, S#Acc, expr.Expr    [S, A]]
    implicit def varSerializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, expr.Expr.Var[S, A]]

    // ---- public ----

    def newConst [S <: Sys[S]](value: A): expr.Expr.Const[S, A]
    def newVar   [S <: Sys[S]](init: expr.Expr[S, A])(implicit tx: S#Tx): expr.Expr.Var[S, A]

    def readConst[S <: Sys[S]](in: DataInput): Expr.Const[S, A]
    def readVar  [S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): expr.Expr.Var[S, A]
  }
}
