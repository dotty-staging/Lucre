/*
 *  ExprTypeImpl.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2015 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr
package impl

import de.sciss.lucre.event.Targets
import de.sciss.lucre.expr
import de.sciss.lucre.stm.{Elem, Copy, Obj, NoSys, Sys}
import de.sciss.serial.{DataInput, DataOutput, Serializer}

import scala.annotation.switch
import scala.language.higherKinds
import scala.language.implicitConversions

trait ExprTypeImpl[A1, Repr[~ <: Sys[~]] <: Expr[~, A1]] extends Type.Expr[A1, Repr] with TypeImpl1[Repr] { self =>
  // ---- public ----

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Ex[S] =
    (in.readByte(): @switch) match {
      case 3 => readIdentifiedConst[S](in, access)
      case 0 =>
        val targets = Targets.Obj.readIdentified[S](in, access)
        in.readByte() match {
          case 0 => readIdentifiedVar[S](in, access, targets)
          case 1 => readNode(in, access, targets)
        }
      case cookie => readCookie(in, access, cookie)
    }

  /** The default implementation reads a type `Int` as operator id `Int`
    * which will be resolved using `readOpExtension`.
    */
  protected def readNode[S <: Sys[S]](in: DataInput, access: S#Acc, targets: Targets.Obj[S])
                                     (implicit tx: S#Tx): Ex[S] = {
    val opID = in.readInt()
    readExtension(op = opID, in = in, access = access, targets = targets)
  }

  /** Reads an identified object whose cookie is neither `3` (constant) nor `0` (node).
    * By default this throws an exception. Sub-classes may use a cookie greater
    * than `3` for other constant types.
    */
  protected def readCookie[S <: Sys[S]](in: DataInput, access: S#Acc, cookie: Byte)(implicit tx: S#Tx): Ex[S] =
    sys.error(s"Unexpected cookie $cookie")

  implicit final def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Ex[S]] /* EventLikeSerializer[S, Repr[S]] */ =
    anySer.asInstanceOf[Ser[S]]

  implicit final def varSerializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Var[S]] /* Serializer[S#Tx, S#Acc, ReprVar[S]] */ =
    anyVarSer.asInstanceOf[VarSer[S]]

  // repeat `implicit` here because IntelliJ IDEA will not recognise it otherwise (SCL-9076)
  implicit final def newConst[S <: Sys[S]](value: A)(implicit tx: S#Tx): Const[S] =
    mkConst[S](tx.newObjID(), value)

  final def newVar[S <: Sys[S]](init: Ex[S])(implicit tx: S#Tx): Var[S] = {
    val targets = Targets.Obj[S]
    val ref     = tx.newVar[Ex[S]](targets.id, init)
    mkVar[S](targets, ref, connect = true)
  }

  protected def mkConst[S <: Sys[S]](id: S#ObjID, value: A)(implicit tx: S#Tx): Const[S]
  protected def mkVar  [S <: Sys[S]](targets: Targets.Obj[S], vr: S#Var[Ex[S]], connect: Boolean)(implicit tx: S#Tx): Var[S]

  final def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Ex[S] =
    serializer[S].read(in, access)

  final def readConst[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Const[S] = {
    val tpe = in.readInt()
    if (tpe != typeID) sys.error(s"Type mismatch, expected $typeID but found $tpe")
    val cookie = in.readByte()
    if (cookie != 3) sys.error(s"Unexpected cookie $cookie")
    readIdentifiedConst(in, access)
  }

  @inline
  private[this] def readIdentifiedConst[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Const[S] = {
    val id      = tx.readObjID(in, access)
    val value   = valueSerializer.read(in)
    mkConst[S](id, value)
  }

  final def readVar[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Var[S] = {
    val tpe = in.readInt()
    if (tpe != typeID) sys.error(s"Type mismatch, expected $typeID but found $tpe")
    val targets = Targets.Obj.read[S](in, access)
    val cookie = in.readByte()
    if (cookie != 0) sys.error(s"Unexpected cookie $cookie")
    readIdentifiedVar(in, access, targets)
  }

  @inline
  private[this] def readIdentifiedVar[S <: Sys[S]](in: DataInput, access: S#Acc, targets: Targets.Obj[S])
                                                  (implicit tx: S#Tx): Var[S] = {
    val ref = tx.readVar[Ex[S]](targets.id, in)
    mkVar[S](targets, ref, connect = false)
  }

  // ---- private ----

  protected trait ConstImpl[S <: Sys[S]] // (val id: S#ID, val constValue: A)
    extends expr.impl.ConstImpl[S, A] {

    final def tpe: Obj.Type = self

    final protected def writeData(out: DataOutput): Unit = valueSerializer.write(constValue, out)

    private[lucre] def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
      mkConst[Out](txOut.newObjID(), constValue)
  }

  protected trait VarImpl[S <: Sys[S]]
    extends expr.impl.VarImpl[S, A, Ex[S]] {

    final def tpe: Obj.Type = self

    private[lucre] def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] = {
      val newTgt = Targets.Obj[Out]
      val newVr  = txOut.newVar(newTgt.id, context(ref()))
      mkVar[Out](newTgt, newVr, connect = true)
    }
  }

  private[this] val anySer    = new Ser   [NoSys]
  private[this] val anyVarSer = new VarSer[NoSys]

  private[this] final class VarSer[S <: Sys[S]] extends Serializer[S#Tx, S#Acc, Var[S]] /* with Reader[S, Var[S]] */ {
    def write(v: Var[S], out: DataOutput): Unit = v.write(out)

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Var[S] = readVar[S](in, access)
  }

  private[this] final class Ser[S <: Sys[S]] extends Serializer[S#Tx, S#Acc, Ex[S]] /* EventLikeSerializer[S, Ex[S]] */ {
    def write(ex: Ex[S], out: DataOutput): Unit = ex.write(out)

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Ex[S] = {
      val tpe = in.readInt()
      if (tpe != typeID) sys.error(s"Type mismatch, expected $typeID but found $tpe")
      readIdentifiedObj(in, access)
    }
  }
}