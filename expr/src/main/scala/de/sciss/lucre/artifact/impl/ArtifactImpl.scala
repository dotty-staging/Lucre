/*
 *  ArtifactImpl.scala
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

package de.sciss.lucre.artifact
package impl

import java.io.File

import de.sciss.lucre.artifact.Artifact.Modifiable
import de.sciss.lucre.event.{EventLike, Targets}
import de.sciss.lucre.expr.List
import de.sciss.lucre.stm.impl.ObjSerializer
import de.sciss.lucre.stm.{NoSys, Obj, Sys}
import de.sciss.lucre.{event => evt}
import de.sciss.model.Change
import de.sciss.serial.{DataInput, DataOutput, Serializer}

object ArtifactImpl {
  import Artifact.Child
  import de.sciss.lucre.artifact.{ArtifactLocation => Location}

  private final val SER_VERSION = 0x4172

  // ---- artifact ----

  def apply[S <: Sys[S]](location: Location[S], child: Child)(implicit tx: S#Tx): Artifact.Modifiable[S] = {
    val targets = evt.Targets[S]
    val _child  = tx.newVar(targets.id, child.path)
    new Impl[S](targets, location, _child)
  }

  def copy[S <: Sys[S]](from: Artifact[S])(implicit tx: S#Tx): Artifact.Modifiable[S] =
    apply[S](from.location, from.child)

  def readIdentifiedArtifact[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Artifact[S] = {
    val targets = Targets.read[S](in, access)
    readArtifact(in, access, targets)
  }

  private def readArtifact[S <: Sys[S]](in: DataInput, access: S#Acc, targets: Targets[S])
                                       (implicit tx: S#Tx): Artifact.Modifiable[S] = {
    val cookie    = in.readShort()
    if (cookie != SER_VERSION) sys.error(s"Version mismatch. Expected $SER_VERSION but found $cookie")
    val location  = readLocation(in, access)
    val _child    = tx.readVar[String](targets.id, in)
    new Impl[S](targets, location, _child)
  }

  def serializer   [S <: Sys[S]]: Serializer[S#Tx, S#Acc, Artifact           [S]] = anySer   .asInstanceOf[Ser   [S]]
  def modSerializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Artifact.Modifiable[S]] = anyModSer.asInstanceOf[ModSer[S]]

  private val anySer    = new Ser   [NoSys]
  private val anyModSer = new ModSer[NoSys]

  private final class Ser[S <: Sys[S]] extends ObjSerializer[S, Artifact[S]] {
    def tpe = Artifact
  }

  private final class ModSer[S <: Sys[S]] extends ObjSerializer[S, Artifact.Modifiable[S]] {
    def tpe = Artifact
  }

  // ---- location ----

  def newLocation[S <: Sys[S]](init: File)(implicit tx: S#Tx): Location.Modifiable[S] = {
    val targets   = evt.Targets[S]
    val directory = tx.newVar(targets.id, init)
    val artifacts = List.Modifiable[S, Artifact[S]]
    new LocationImpl[S](targets, directory, artifacts)
  }

  def readLocation[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Location[S] =
    locationSerializer[S].read(in, access)

  def readModLocation[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Location.Modifiable[S] =
    modLocationSerializer[S].read(in, access)

  def locationSerializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Location[S]] = anyLocSer.asInstanceOf[LocSer[S]]

  def modLocationSerializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Location.Modifiable[S]] =
    anyModLocSer.asInstanceOf[ModLocSer[S]]

  def readIdentifiedLocation[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Location[S] = {
    val targets = Targets.read[S](in, access)
    readLoc(in, access, targets)
  }

  private val anyLocSer     = new LocSer[NoSys]
  private val anyModLocSer  = new ModLocSer[NoSys]

  private def readLoc[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                  (implicit tx: S#Tx): Location.Modifiable[S] = {
    val cookie    = in.readShort()
    require(cookie == SER_VERSION, s"Version mismatch. Expected $SER_VERSION but found $cookie")
    val directory = tx.readVar[File](targets.id, in)
    val artifacts = List.Modifiable.read[S, Artifact[S]](in, access)
    new LocationImpl[S](targets, directory, artifacts)
  }

  private final class LocSer[S <: Sys[S]] extends ObjSerializer[S, Location[S]] {
    def tpe = Location
  }

  private final class ModLocSer[S <: Sys[S]] extends ObjSerializer[S, Location.Modifiable[S]] {
    def tpe = Location
  }

  private final class LocationImpl[S <: Sys[S]](protected val targets: evt.Targets[S],
                                                    _directory: S#Var[File],
                                                    artifacts: List.Modifiable[S, Artifact[S]])
    extends Location.Modifiable[S]
    with evt.impl.SingleNode[S, Location.Update[S]] { loc =>

    def tpe: Obj.Type = Location

    override def toString = s"ArtifactLocation$id"

    def iterator(implicit tx: S#Tx): Iterator[Artifact[S]] = artifacts.iterator

    def modifiableOption: Option[Location.Modifiable[S]] = Some(this)

    def directory(implicit tx: S#Tx): File = _directory()
    def directory_=(value: File)(implicit tx: S#Tx): Unit = {
      val change = Change(_directory(), value)
      if (change.isSignificant) {
        _directory() = value
        changed.fire(Location.Moved(loc, change))
      }
    }

    object changed extends Changed
      with evt.impl.Generator[S, Location.Update[S]]
      with evt.impl.Root     [S, Location.Update[S]]

    def remove(artifact: Artifact[S])(implicit tx: S#Tx): Unit = {
      val idx = artifacts.indexOf(artifact)
      if (!artifacts.remove(artifact)) throw new NoSuchElementException(s"Artifact $artifact was not found in the store")
      changed.fire(Location.Removed(loc, idx, artifact))
    }

    def add(file: File)(implicit tx: S#Tx): Artifact.Modifiable[S] = {
      val base      = _directory()
      val child     = Artifact.relativize(base, file)
      val artifact  = ArtifactImpl.apply(loc, child)
      val idx       = artifacts.size
      artifacts.addLast(artifact)
      changed.fire(Location.Added(loc, idx, artifact))
      artifact
    }

    protected def writeData(out: DataOutput): Unit = {
      out.writeShort(SER_VERSION)
      _directory.write(out)
      artifacts .write(out)
    }

    protected def disposeData()(implicit tx: S#Tx): Unit = {
      _directory.dispose()
      artifacts .dispose()
    }
  }

  private final class Impl[S <: Sys[S]](protected val targets: evt.Targets[S],
                                        val location: Location[S], _child: S#Var[String])
    extends Artifact.Modifiable[S]
    with evt.impl.MappingNode[S, Change[File], Location.Update[S]]
    with evt.impl.SingleNode[S, Change[File]] {

    def tpe: Obj.Type = Artifact

    override def toString = s"Artifact$id"

    def modifiableOption: Option[Modifiable[S]] = Some(this)

    def child(implicit tx: S#Tx): Child = Child(_child())

    def child_=(value: Child)(implicit tx: S#Tx): Unit = {
      val oldP  = _child()
      val newP  = value.path
      if (oldP != newP) {
        val base    = location.directory
        _child()    = newP
        val change  = Change(new File(base, oldP), new File(base, newP))
        changed.fire(change)
      }
    }

    object changed extends Changed with Mapped

    protected def inputEvent: EventLike[S, Location.Update[S]] = location.changed

    protected def foldUpdate(generated: Option[Change[File]],
                             input: Location.Update[S])(implicit tx: S#Tx): Option[Change[File]] =
      generated.orElse {
        input match {
          case Location.Moved(_, Change(oldBase, newBase)) =>
            val path    = _child()
            val change  = Change(new File(oldBase, path), new File(newBase, path))
            Some(change)
          case _ => None
        }
      }

    def value(implicit tx: S#Tx): Artifact.Value = {
      val base   = location.directory
      val child  = _child()
      new File(base, child)
    }

    protected def disposeData()(implicit tx: S#Tx): Unit = {
      _child.dispose()
    }

    protected def writeData(out: DataOutput): Unit = {
      out.writeShort(SER_VERSION)
      location.write(out)
      _child  .write(out)
    }
  }
}