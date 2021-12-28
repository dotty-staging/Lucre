/*
 *  ExprTypeImpl.scala
 *  (Lucre 4)
 *
 *  Copyright (c) 2009-2021 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre
package impl

import de.sciss.lucre
import de.sciss.lucre.Event.Targets
import de.sciss.lucre.Log.{event => logEvent}
import de.sciss.lucre.expr.graph.Ex
import de.sciss.lucre.expr.{Context, ExElem}
import de.sciss.lucre.{Var => LVar}
import de.sciss.model.Change
import de.sciss.serial.{DataInput, DataOutput, TFormat, WritableFormat}

import scala.annotation.{switch, unused}
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.language.implicitConversions

trait ExprTypeImpl[A1, Repr[~ <: Txn[~]] <: Expr[~, A1]]
  extends Expr.Type[A1, Repr] { self =>

  implicit final def tpe: Expr.Type[A1, Repr] = this

//  type Ext = ExprTypeExtension1[Repr]

  protected def mkExtArray(size: Int): Array[ExprTypeExtension1[Repr]] = new Array(size)

  private[this] var extensions = mkExtArray(0) // new Array[Ext](0)

  final protected def addExtension(extensions: Array[ExprTypeExtension1[Repr]],
                                   ext: ExprTypeExtension1[Repr]): Array[ExprTypeExtension1[Repr]] = {
    val opLo = ext.opLo
    val opHi = ext.opHi
    require (opLo <= opHi, s"Lo ($opLo) must be less than or equal hi ($opHi)")
    val idx0  = extensions.indexWhere(_.opLo > opHi)
    val len   = extensions.length
    val idx   = if (idx0 < 0) len else idx0
    if (idx > 0) {
      val pred = extensions(idx - 1)
      require(pred.opHi < opLo, s"Extension overlap for $pred versus $ext")
    }
    val extensions1 = mkExtArray(len + 1) // new Array[Ext](len + 1)
    if (idx > 0) System.arraycopy(extensions, 0, extensions1, 0, idx)
    extensions1(idx) = ext
    if (idx < len) System.arraycopy(extensions, idx, extensions1, idx + 1, len - idx)
    extensions1
  }

  final protected def findExt(extensions: Array[ExprTypeExtension1[Repr]], op: Int): ExprTypeExtension1[Repr] = {
    var index = 0
    var low   = 0
    var high  = extensions.length - 1
    while ({
      index = (high + low) >> 1
      low  <= high
    }) {
      val ext = extensions(index)
      if (ext.opLo <= op) {
        if (ext.opHi >= op) return ext
        low = index + 1
      } else {
        high = index - 1
      }
    }
    null
  }

  final def registerExtension(ext: ExprTypeExtension1[Repr]): Unit = extensions = addExtension(extensions, ext)

  final protected def findExt(op: Int): ExprTypeExtension1[Repr] = findExt(extensions, op)

  final protected def readExtension[T <: Txn[T]](op: Int, in: DataInput, targets: Targets[T])
                                                (implicit tx: T): Repr[T] = {
    val ext = findExt(op)
    if (ext == null) sys.error(s"Unknown extension operator $op")
    ext.readExtension[T](op, in, targets)
  }

  override def readIdentifiedObj[T <: Txn[T]](in: DataInput)(implicit tx: T): E[T] =
    (in.readByte(): @switch) match {
      case 3 => readIdentifiedConst[T](in)
      case 0 =>
        val targets = Event.Targets.readIdentified[T](in)
        in.readByte() match {
          case 0 => readIdentifiedVar[T](in, targets)
          case 1 => readNode(in, targets)
          case 2 => readIdentifiedProgram[T](in, targets)
        }
      case cookie => readCookie(in, cookie)
    }

  /** The default implementation reads a type `Int` as operator id `Int`
   * which will be resolved using `readOpExtension`.
   */
  protected def readNode[T <: Txn[T]](in: DataInput, targets: Event.Targets[T])
                                     (implicit tx: T): E[T] = {
    val opId = in.readInt()
    readExtension(op = opId, in = in, targets = targets)
  }

  /** Reads an identified object whose cookie is neither `3` (constant) nor `0` (node).
   * By default this throws an exception. Sub-classes may use a cookie greater
   * than `3` for other constant types.
   */
  protected def readCookie[T <: Txn[T]](@unused in: DataInput, cookie: Byte)(implicit tx: T): E[T] =  // sub-class may need tx
    sys.error(s"Unexpected cookie $cookie")

  implicit final def format[T <: Txn[T]]: TFormat[T, E[T]] /* EventLikeFormat[S, Repr[T]] */ =
    anyFmt.asInstanceOf[Fmt[T]]

  implicit final def varFormat[T <: Txn[T]]: TFormat[T, Var[T]] /* Format[T, S#Acc, ReprVar[T]] */ =
    anyVarFmt.asInstanceOf[VarFmt[T]]

  // repeat `implicit` here because IntelliJ IDEA will not recognise it otherwise (SCL-9076)
  implicit final def newConst[T <: Txn[T]](value: A)(implicit tx: T): Const[T] =
    mkConst[T](tx.newId(), value)

  final def newVar[T <: Txn[T]](init: E[T])(implicit tx: T): Var[T] = {
    val targets = Event.Targets[T]()
    val ref     = targets.id.newVar[E[T]](init)
    mkVar[T](targets, ref, connect = true)
  }

  final def newProgram[T <: Txn[T]](program: Ex[A])(implicit tx: T): Program[T] = {
    val targets = Event.Targets[T]()
    val id      = targets.id
    val sources = id.newVar(Vec.empty[Event[T, Any]])
    val value   = id.newVar[A](null.asInstanceOf[A])
    mkProgram[T](targets, program = program, sources = sources, value = value, connect = true)
  }

  protected def mkConst[T <: Txn[T]](id: Ident[T], value: A)(implicit tx: T): Const[T]

  protected def mkVar[T <: Txn[T]](targets: Event.Targets[T], vr: lucre.Var[T, E[T]], connect: Boolean)
                                  (implicit tx: T): Var[T]

  protected def mkProgram[T <: Txn[T]](targets: Event.Targets[T],
                                       program: Ex[A],
                                       sources: LVar[T, Vec[Event[T, Any]]],
                                       value  : LVar[T, A],
                                       connect: Boolean)
                                      (implicit tx: T): Program[T]

  override final def read[T <: Txn[T]](in: DataInput)(implicit tx: T): E[T] =
    format[T].readT(in)

  override final def readConst[T <: Txn[T]](in: DataInput)(implicit tx: T): Const[T] = {
    val tpe = in.readInt()
    if (tpe != typeId) sys.error(s"Type mismatch, expected $typeId but found $tpe")
    val cookie = in.readByte()
    if (cookie != 3) sys.error(s"Unexpected cookie $cookie")
    readIdentifiedConst(in)
  }

  @inline
  private[this] def readIdentifiedConst[T <: Txn[T]](in: DataInput)(implicit tx: T): Const[T] = {
    val id      = tx.readId(in)
    val value   = valueFormat.read(in)
    mkConst[T](id, value)
  }

  override final def readVar[T <: Txn[T]](in: DataInput)(implicit tx: T): Var[T] = {
    val tpe = in.readInt()
    if (tpe != typeId) sys.error(s"Type mismatch, expected $typeId but found $tpe")
    val targets = Event.Targets.read[T](in)
    val cookie = in.readByte()
    if (cookie != 0) sys.error(s"Unexpected cookie $cookie")
    readIdentifiedVar(in, targets)
  }

  @inline
  private[this] def readIdentifiedVar[T <: Txn[T]](in: DataInput, targets: Event.Targets[T])
                                                  (implicit tx: T): Var[T] = {
    val ref = targets.id.readVar[E[T]](in)
    mkVar[T](targets, ref, connect = false)
  }

  private[this] final val PROGRAM_SER_VERSION = 0x4945

  @inline
  private[this] def readIdentifiedProgram[T <: Txn[T]](in: DataInput, targets: Event.Targets[T])
                                                      (implicit tx: T): Program[T] = {
    val id      = targets.id
    val cookie  = in.readShort().toInt
    if (cookie != PROGRAM_SER_VERSION) {
      sys.error(s"Unexpected cookie ${cookie.toHexString} instead of ${PROGRAM_SER_VERSION.toHexString}")
    }
    val ref     = new ExElem.RefMapIn(in)
    val ex      = ref.readEx[A]()
    val sources = id.readVar[Vec[Event[T, Any]]](in)
    val value   = id.readVar[A](in)
    mkProgram[T](targets, program = ex, sources = sources, value = value, connect = false)
  }

  // ---- private ----

  protected trait ConstImpl[T <: Txn[T]] // (val id: S#Id, val constValue: A)
    extends ExprConstImpl[T, A] {

    final def tpe: Obj.Type = self

    final protected def writeData(out: DataOutput): Unit = valueFormat.write(constValue, out)

    private[lucre] def copy[Out <: Txn[Out]]()(implicit txIn: T, txOut: Out, context: Copy[T, Out]): Elem[Out] =
      newConst[Out](constValue)
  }

  protected trait VarImpl[T <: Txn[T]]
    extends ExprVarImpl[T, A, E[T]] {

    final override def tpe: Obj.Type = self

    private[lucre] override def copy[Out <: Txn[Out]]()(implicit txIn: T, txOut: Out,
                                                        context: Copy[T, Out]): Elem[Out] =
      newVar[Out](context(ref()))
  }

  protected trait ProgramImpl[T <: Txn[T]] extends Expr.Program[T, A] with ExprNodeImpl[T, A] {
    // ---- abstract ---

    protected def sourcesRef: LVar[T, Vec[Event[T, Any]]]
    protected def valueRef  : LVar[T, A]

    // ---- impl ----

    final def connect()(implicit tx: T): this.type = {
      valueImpl
      this
    }

    override final def value(implicit tx: T): A = {
      val valueNew = valueImpl
      val valueOld = valueRef()
      if (valueNew != valueOld) valueRef() = valueNew
      valueNew
    }

    private def valueImpl(implicit tx: T): A = {
      val hc = Context.headless[T](this)
      implicit val ctx: Context[T] = hc
      val peer      = program.expand[T]
      val res       = peer.value
      val eventsOld = sourcesRef()
      val eventsNew = hc.events
      if (eventsOld != eventsNew) {
        sourcesRef()  = eventsNew
        val eventsAdd = eventsNew diff eventsOld
        val eventsRem = eventsOld diff eventsNew
        // logEvent.debug(s"ExObj remove $eventsRem")
        // logEvent.debug(s"ExObj add    $eventsAdd")
        eventsRem.foreach(_ -/-> changed)
        eventsAdd.foreach(_ ---> changed)
      }
      res
    }

    final override def tpe: Obj.Type = self

    object changed extends Changed with GeneratorEvent[T, Change[A]] with Caching {
      private[lucre] def pullUpdate(pull: Pull[T])(implicit tx: T): Option[Change[A]] = {
        val valueOld  = valueRef()
        val valueNew  = value  // updates cache
        logEvent.debug(s"ExObj pullUpdate; $valueOld -> $valueNew")
        val ch        = Change(valueOld, valueNew)
        ch.toOption
      }
    }

    private def writeEx(out: DataOutput): Unit = {
      val ref = new ExElem.RefMapOut(out)
      ref.writeElem(program)
    }

    override protected def writeData(out: DataOutput): Unit = {
      out.writeByte(2)  // 'program'
      out.writeShort(PROGRAM_SER_VERSION)
      writeEx(out)
      sourcesRef.write(out)
      valueRef  .write(out)
    }

    override protected def disposeData()(implicit tx: T): Unit =
      sourcesRef.swap(Vector.empty).foreach(_ -/-> changed)

    /** Makes a deep copy of an element, possibly translating it to a different system `Out`. */
    override private[lucre] def copy[Out <: Txn[Out]]()(implicit txIn: T, txOut: Out,
                                                        context: Copy[T, Out]): Elem[Out] =
      newProgram[Out](program)
  }

  private[this] val anyFmt    = new Fmt   [AnyTxn]
  private[this] val anyVarFmt = new VarFmt[AnyTxn]

  private[this] final class VarFmt[T <: Txn[T]] extends WritableFormat[T, Var[T]] {
    override def readT(in: DataInput)(implicit tx: T): Var[T] = readVar[T](in)
  }

  private[this] final class Fmt[T <: Txn[T]] extends WritableFormat[T, E[T]] {
    override def readT(in: DataInput)(implicit tx: T): E[T] = {
      val tpe = in.readInt()
      if (tpe != typeId) sys.error(s"Type mismatch, expected $typeId but found $tpe")
      readIdentifiedObj(in)
    }
  }
}
