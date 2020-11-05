/*
 *  ArtifactImpl.scala
 *  (Lucre 4)
 *
 *  Copyright (c) 2009-2020 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre
package impl

import de.sciss.lucre.Artifact.{Child, Modifiable, Value}
import de.sciss.lucre.Event.Targets
import de.sciss.lucre.{ArtifactLocation => Location}
import de.sciss.model.Change
import de.sciss.serial.{DataInput, DataOutput, TFormat}

object ArtifactImpl extends ArtifactImplPlatform {

  private final val SER_VERSION = 0x4172

  // ---- artifact ----

  def apply[T <: Txn[T]](location: Location[T], child: Child)(implicit tx: T): Artifact.Modifiable[T] = {
    val targets = Targets[T]()
    val id      = targets.id
    val _child  = id.newVar(child.path)
    new Impl[T](targets, location, _child) // .connect()
  }

  def copy[T <: Txn[T]](from: Artifact[T])(implicit tx: T): Artifact.Modifiable[T] =
    apply[T](from.location, from.child)

  def readIdentifiedArtifact[T <: Txn[T]](in: DataInput)(implicit tx: T): Artifact[T] = {
    val targets = Targets.read[T](in)
    readArtifact(in, targets)
  }

  private def readArtifact[T <: Txn[T]](in: DataInput, targets: Targets[T])
                                       (implicit tx: T): Artifact.Modifiable[T] = {
    val cookie    = in.readShort()
    if (cookie != SER_VERSION) sys.error(s"Version mismatch. Expected $SER_VERSION but found $cookie")
    val location  = Location.read(in)
    val id        = targets.id
    val _child    = id.readVar[String](in)
    new Impl[T](targets, location, _child)
  }

  def format   [T <: Txn[T]]: TFormat[T, Artifact           [T]] = anyFmt   .cast
  def modFormat[T <: Txn[T]]: TFormat[T, Artifact.Modifiable[T]] = anyModFmt.cast

  private val anyFmt    = new Fmt   [AnyTxn]
  private val anyModFmt = new ModFmt[AnyTxn]

  private final class Fmt[T <: Txn[T]] extends ObjCastFormat[T, Artifact] {
    def tpe: Obj.Type = Artifact
  }

  private final class ModFmt[T <: Txn[T]] extends ObjCastFormat[T, Artifact.Modifiable] {
    def tpe: Obj.Type = Artifact
  }

  private final class Impl[T <: Txn[T]](protected val targets: Targets[T],
                                        val location: Location[T], _child: Var[T, String])
    extends Artifact.Modifiable[T]
      with MappingEventNode[T, Change[Value], Change[Value]]
      with SingleEventNode[T, Change[Value]] {

    def tpe: Obj.Type = Artifact

    override def toString = s"Artifact$id"

    private[lucre] override def copy[Out <: Txn[Out]]()(implicit txIn: T, txOut: Out, context: Copy[T, Out]): Elem[Out] =
      ArtifactImpl(context(location), child)

    def modifiableOption: Option[Modifiable[T]] = Some(this)

    def child(implicit tx: T): Child = Child(_child())

    def child_=(value: Child)(implicit tx: T): Unit = {
      val oldP  = _child()
      val newP  = value.path
      if (oldP != newP) {
        val base    = location.value // directory
        _child()    = newP
        val change  = Change(Value.append(base, oldP), Value.append(base, newP))
        changed.fire(change)(tx)
      }
    }

    object changed extends Changed with Mapped

    protected def inputEvent: EventLike[T, Change[Value]] = location.changed

    protected def foldUpdate(generated: Option[Change[Value]], input: Change[Value])
                            (implicit tx: T): Option[Change[Value]] =
      generated.orElse {
        input match {
          case Change(oldBase, newBase) =>
            // case Location.Moved(_, Change(oldBase, newBase)) =>
            val path    = _child()
            val change  = Change(Value.append(oldBase, path), Value.append(newBase, path))
            Some(change)
          case _ => None
        }
      }

    def value(implicit tx: T): Value = {
      val base   = location.value // directory
      val child  = _child()
      Value.append(base, child)
    }

    protected def disposeData()(implicit tx: T): Unit =
      _child.dispose()

    protected def writeData(out: DataOutput): Unit = {
      out.writeShort(SER_VERSION)
      location.write(out)
      _child  .write(out)
    }
  }
}