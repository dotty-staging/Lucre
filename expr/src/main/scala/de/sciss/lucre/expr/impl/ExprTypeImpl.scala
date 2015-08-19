package de.sciss.lucre.expr
package impl

import de.sciss.lucre.event.{Node, Targets}
import de.sciss.lucre.stm.{NoSys, Sys}
import de.sciss.model
import de.sciss.serial.{Serializer, DataOutput, DataInput}

import scala.annotation.switch

trait ExprTypeImpl[A] extends Type.Expr[A] with TypeImpl1[Repr[A]#L] { tpe =>
  final protected type Ex [S <: Sys[S]] = Expr     [S, A]
  final protected type ExN[S <: Sys[S]] = Expr.Node[S, A]
  final protected type ExV[S <: Sys[S]] = Expr.Var [S, A]
  final protected type Change = model.Change[A]

//  // ---- abstract ----
//
//  protected def readNode[S <: Sys[S]](cookie: Int, in: DataInput, access: S#Acc, targets: Targets[S])
//                                     (implicit tx: S#Tx): Ex[S] with Node[S]

  // ---- public ----

  /** The default implementation reads a type `Int` requiring to match `typeID`, followed by an operator id `Int`
    * which will be resolved using `readOpExtension`.
    */
  protected def readNode[S <: Sys[S]](in: DataInput, access: S#Acc, targets: Targets[S])
                                     (implicit tx: S#Tx): Ex[S] with Node[S] = {
    val tpe  = in.readInt()
    if (tpe != typeID) sys.error(s"Invalid type id (found $tpe, required $typeID)")
    val opID = in.readInt()
    readExtension(/* cookie, */ op = opID, in = in, access = access, targets = targets)
  }

  implicit final def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Ex[S]] /* EventLikeSerializer[S, Repr[S]] */ =
    anySer.asInstanceOf[Ser[S]]

  implicit final def varSerializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, ExV[S]] /* Serializer[S#Tx, S#Acc, ReprVar[S]] */ =
    anyVarSer.asInstanceOf[VarSer[S]]

  final def newConst[S <: Sys[S]](value: A): Expr.Const[S, A] = new Const(value)

  final def newVar[S <: Sys[S]](init: Ex[S])(implicit tx: S#Tx): ExV[S] = {
    val targets = Targets[S]
    val ref     = tx.newVar[Ex[S]](targets.id, init)
    new Var[S](ref, targets)
  }

  final def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Ex[S] =
    serializer[S].read(in, access)

  final def readConst[S <: Sys[S]](in: DataInput): Expr.Const[S, A] = {
    val cookie = in.readByte()
    if (cookie != 3) sys.error(s"Unexpected cookie $cookie") // XXX TODO cookie should be available in lucre.event
    newConst[S](readValue(in))
  }

  final def readVar[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): ExV[S] = {
    val targets = Targets.read[S](in, access)
    val cookie = in.readByte()
    if (cookie != 0) sys.error(s"Unexpected cookie $cookie")
    val ref = tx.readVar[Ex[S]](targets.id, in)
    new Var[S](ref, targets)
  }

  final protected def readVar[S <: Sys[S]](in: DataInput, access: S#Acc, targets: Targets[S])
                                          (implicit tx: S#Tx): ExV[S] = {
    val ref = tx.readVar[Ex[S]](targets.id, in)
    new Var[S](ref, targets).connect()
  }

  // ---- private ----

  private[this] final case class Const[S <: Sys[S]](constValue: A) extends ConstImpl[S, A] {
    protected def writeData(out: DataOutput): Unit = writeValue(constValue, out)
  }

  private[this] final class Var[S <: Sys[S]](protected val ref: S#Var[Ex[S]], protected val targets: Targets[S])
    extends VarImpl[S, A] {

    def typeID: Int = tpe.typeID
  }

  private[this] val anySer    = new Ser   [NoSys]
  private[this] val anyVarSer = new VarSer[NoSys]

  private[this] final class VarSer[S <: Sys[S]] extends Serializer[S#Tx, S#Acc, ExV[S]] /* with Reader[S, ExV[S]] */ {
    def write(v: ExV[S], out: DataOutput): Unit = v.write(out)

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): ExV[S] = readVar[S](in, access)

//    def read(in: DataInput, access: S#Acc, targets: Targets[S])(implicit tx: S#Tx): ExV[S] with Node[S] =
//      readVar[S](in, access, targets)
  }

  private[this] final class Ser[S <: Sys[S]] extends Serializer[S#Tx, S#Acc, Ex[S]] /* EventLikeSerializer[S, Ex[S]] */ {
    def write(ex: Ex[S], out: DataOutput): Unit = ex.write(out)

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Ex[S] = {
      (in.readByte(): @switch) match {
        case 3 => readConstant(in)
        case 0 =>
          val targets = Targets.readIdentified[S](in, access)
          read(in, access, targets)
        case cookie => sys.error(s"Unexpected cookie $cookie")
      }
    }

    def read(in: DataInput, access: S#Acc, targets: Targets[S])(implicit tx: S#Tx): Ex[S] with Node[S] = {
      // 0 = var, 1 = op
      in.readByte() match {
        case 0 => readVar (in, access, targets)
        case 1 => readNode(in, access, targets)
      }
    }

    def readConstant(in: DataInput)(implicit tx: S#Tx): Ex[S] = newConst(readValue(in))
  }
}