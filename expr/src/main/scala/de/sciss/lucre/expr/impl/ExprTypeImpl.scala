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
import de.sciss.lucre.stm.{Obj, NoSys, Sys}
import de.sciss.model
import de.sciss.serial.{DataInput, DataOutput, Serializer}

import scala.annotation.switch

trait ExprTypeImpl[A] extends Type.Expr[A] with TypeImpl1[Repr[A]#L] { self =>
  final protected type Ex [S <: Sys[S]] = Expr      [S, A]
  final protected type ExV[S <: Sys[S]] = Expr.Var  [S, A]
  final protected type ExC[S <: Sys[S]] = Expr.Const[S, A]
  final protected type Change = model.Change[A]

//  // ---- abstract ----
//
//  protected def readNode[S <: Sys[S]](cookie: Int, in: DataInput, access: S#Acc, targets: Targets[S])
//                                     (implicit tx: S#Tx): Ex[S] with Node[S]

  // ---- public ----

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Ex[S] =
    (in.readByte(): @switch) match {
      case 3 => readIdentifiedConst(in, access)
      case 0 =>
        val targets = Targets.readIdentified[S](in, access)
        in.readByte() match {
          case 0 => readIdentifiedVar (in, access, targets)
          case 1 => readNode(in, access, targets)
        }
      case cookie => sys.error(s"Unexpected cookie $cookie")
    }

  /** The default implementation reads a type `Int` as operator id `Int`
    * which will be resolved using `readOpExtension`.
    */
  protected def readNode[S <: Sys[S]](in: DataInput, access: S#Acc, targets: Targets[S])
                                     (implicit tx: S#Tx): Ex[S] = {
    // val tpe  = in.readInt()
    // if (tpe != typeID) sys.error(s"Invalid type id (found $tpe, required $typeID)")
    val opID = in.readInt()
    readExtension(/* cookie, */ op = opID, in = in, access = access, targets = targets)
  }

  implicit final def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Ex[S]] /* EventLikeSerializer[S, Repr[S]] */ =
    anySer.asInstanceOf[Ser[S]]

  implicit final def varSerializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, ExV[S]] /* Serializer[S#Tx, S#Acc, ReprVar[S]] */ =
    anyVarSer.asInstanceOf[VarSer[S]]

  final def newConst[S <: Sys[S]](value: A)(implicit tx: S#Tx): ExC[S] = 
    new Const(tx.newID(), value)

  final def newVar[S <: Sys[S]](init: Ex[S])(implicit tx: S#Tx): ExV[S] = {
    val targets = Targets[S]
    val ref     = tx.newVar[Ex[S]](targets.id, init)
    new Var[S](ref, targets)
  }

  final def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Ex[S] =
    serializer[S].read(in, access)

  final def readConst[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): ExC[S] = {
    val cookie = in.readByte()
    if (cookie != 3) sys.error(s"Unexpected cookie $cookie")
    readIdentifiedConst(in, access)
  }

  @inline
  private[this] def readIdentifiedConst[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): ExC[S] = {
    val id      = tx.readID(in, access)
    val value   = valueSerializer.read(in)
    new Const(id, value)
  }

  final def readVar[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): ExV[S] = {
    val targets = Targets.read[S](in, access)
    val cookie = in.readByte()
    if (cookie != 0) sys.error(s"Unexpected cookie $cookie")
    readIdentifiedVar(in, access, targets)
  }

  @inline
  private[this] def readIdentifiedVar[S <: Sys[S]](in: DataInput, access: S#Acc, targets: Targets[S])
                                                  (implicit tx: S#Tx): ExV[S] = {
    val ref = tx.readVar[Ex[S]](targets.id, in)
    new Var[S](ref, targets)
  }

  // ---- private ----

  private[this] final class Const[S <: Sys[S]](val id: S#ID, val constValue: A) extends ConstImpl[S, A] {
    def tpe: Obj.Type = self

    protected def writeData(out: DataOutput): Unit = valueSerializer.write(constValue, out)
  }

  private[this] final class Var[S <: Sys[S]](protected val ref: S#Var[Ex[S]], protected val targets: Targets[S])
    extends VarImpl[S, A] {

    def tpe: Obj.Type = self
  }

  private[this] val anySer    = new Ser   [NoSys]
  private[this] val anyVarSer = new VarSer[NoSys]

  private[this] final class VarSer[S <: Sys[S]] extends Serializer[S#Tx, S#Acc, ExV[S]] /* with Reader[S, ExV[S]] */ {
    def write(v: ExV[S], out: DataOutput): Unit = v.write(out)

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): ExV[S] = readVar[S](in, access)
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