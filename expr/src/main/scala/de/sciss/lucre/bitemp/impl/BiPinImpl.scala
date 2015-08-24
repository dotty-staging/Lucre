/*
 *  BiPinImpl.scala
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

package de.sciss.lucre.bitemp
package impl

import de.sciss.lucre.data.SkipList
import de.sciss.lucre.event.{EventLike, Targets}
import de.sciss.lucre.expr.{Expr, LongObj}
import de.sciss.lucre.stm.impl.ObjSerializer
import de.sciss.lucre.stm.{Elem, NoSys, Obj, Sys}
import de.sciss.lucre.{event => evt, stm}
import de.sciss.model.Change
import de.sciss.serial.{DataInput, DataOutput, Serializer}

import scala.collection.breakOut
import scala.collection.immutable.{IndexedSeq => Vec}

object BiPinImpl {
  import BiPin.{Added, Entry, Leaf, Modifiable, Moved, Removed, Update}

  private type Tree[S <: Sys[S], A] = SkipList.Map[S, Long, Leaf[S, A]]

  def newEntry[S <: Sys[S], A <: Elem[S]](key: LongObj[S], value: A)(implicit tx: S#Tx): Entry[S, A] =
    if (Expr.isConst(key)) new ConstEntry(            key, value)
    else                   new NodeEntry (Targets[S], key, value).connect()

  def readEntry[S <: Sys[S], A <: Elem[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Entry[S, A] = {
    val tpe = in.readInt()
    if (tpe != Entry.typeID) sys.error(s"Type mismatch. Found $tpe, expected ${Entry.typeID}")
    in.readByte() match {
      case 3 =>
        val key     = LongObj.read(in, access)
        val value   = Elem.read(in, access).asInstanceOf[A]
        new ConstEntry(key, value)

      case 0 =>
        val targets = Targets.readIdentified[S](in, access)
        readEntry(in, access, targets)
    }
  }

  def entrySerializer[S <: Sys[S], A <: Elem[S]]: Serializer[S#Tx, S#Acc, Entry[S, A]] =
    anyEntrySer.asInstanceOf[EntrySer[S, A]]

  private val anyEntrySer = new EntrySer[NoSys, Obj[NoSys]]

  private final class EntrySer[S <: Sys[S], A <: Elem[S]] extends Serializer[S#Tx, S#Acc, Entry[S, A]] {
    def write(e: Entry[S, A], out: DataOutput): Unit = e.write(out)

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Entry[S, A] = readEntry(in, access)
  }

  def readIdentifiedEntry[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Elem[S] = {
    val targets = Targets.read[S](in, access)
    readEntry(in, access, targets)
  }

  private def readEntry[S <: Sys[S], A <: Elem[S]](in: DataInput, access: S#Acc, targets: Targets[S])
                                                 (implicit tx: S#Tx): Entry[S, A] = {
    val key     = LongObj.read(in, access)
    val value   = Elem.read(in, access).asInstanceOf[A]
    new NodeEntry(targets, key, value)
  }

  private trait EntryImpl[S <: Sys[S], A <: Elem[S]] extends Entry[S, A] {
    final def tpe: Elem.Type = Entry

    final protected def writeData(out: DataOutput): Unit = {
      key  .write(out)
      value.write(out)
    }
  }

  private case class ConstEntry[S <: Sys[S], A <: Elem[S]](key: LongObj[S], value: A)
    extends EntryImpl[S, A] with stm.impl.ConstElemImpl[S] {

    def changed: EventLike[S, Change[Long]] = evt.Dummy[S, Change[Long]]
  }

  private final class NodeEntry[S <: Sys[S], A <: Elem[S]](protected val targets: Targets[S],
                                                           val key: LongObj[S], val value: A)
    extends EntryImpl[S, A] with evt.impl.SingleNode[S, Change[Long]] {

    // bueno, it's not really the "root", but the type is the same
    object changed extends Changed with evt.impl.Root[S, Change[Long]]

    protected def disposeData()(implicit tx: S#Tx) = disconnect()

    def connect()(implicit tx: S#Tx): this.type = {
      key.changed ---> changed
      this
    }

    private[this] def disconnect()(implicit tx: S#Tx): Unit = {
      key.changed -/-> changed
    }
  }

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] = {
    val targets = Targets.read(in, access)
    BiPinImpl.readImpl(in, access, targets)
  }

  private implicit def leafSerializer[S <: Sys[S], A <: Elem[S]]: Serializer[S#Tx, S#Acc, Leaf[S, A]] =
    anyLeafSer.asInstanceOf[LeafSer[S, A]]

  private val anyLeafSer = new LeafSer[NoSys, Obj[NoSys]]

  private final class LeafSer[S <: Sys[S], A <: Elem[S]] extends Serializer[S#Tx, S#Acc, Leaf[S, A]] {
    def write(leaf: BiPin.Leaf[S, A], out: DataOutput): Unit = {
      val sz = leaf.size
      out.writeInt(sz)
      if (sz == 0) return
      leaf.foreach(_.write(out))
    }

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): BiPin.Leaf[S, A] = {
      val sz = in.readInt()
      if (sz == 0) Vector.empty
      else Vector.fill(sz)(readEntry(in, access))
    }
  }

  def newModifiable[S <: Sys[S], A <: Elem[S]](implicit tx: S#Tx): Modifiable[S, A] = {
    val tree: Tree[S, A] = SkipList.Map.empty[S, Long, Leaf[S, A]]()
    new Impl(evt.Targets[S], tree)
  }

  def serializer[S <: Sys[S], A <: Elem[S]]: Serializer[S#Tx, S#Acc, BiPin[S, A]] =
    anySer.asInstanceOf[Ser[S, A, BiPin[S, A]]]

  def modifiableSerializer[S <: Sys[S], A <: Elem[S]]: Serializer[S#Tx, S#Acc, BiPin.Modifiable[S, A]] =
    anySer.asInstanceOf[Ser[S, A, BiPin.Modifiable[S, A]]]

  private val anySer = new Ser[NoSys, Obj[NoSys], BiPin[NoSys, Obj[NoSys]]]

  private def readImpl[S <: Sys[S], A <: Elem[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                                (implicit tx: S#Tx): Impl[S, A] = {
    val tree: Tree[S, A] = SkipList.Map.read[S, Long, Leaf[S, A]](in, access)
    new Impl(targets, tree)
  }

  private class Ser[S <: Sys[S], A <: Elem[S], Repr >: Impl[S, A] <: BiPin[S, A]] extends ObjSerializer[S, Repr] {
    def tpe = BiPin
  }

  private final class Impl[S <: Sys[S], A](protected val targets: evt.Targets[S], tree: Tree[S, A])
    extends Modifiable[S, A]
    with evt.impl.SingleNode[S, Update[S, A]] {
    pin =>

    def tpe: Obj.Type = BiPin

    override def toString: String = s"BiPin${tree.id}"

    def modifiableOption: Option[BiPin.Modifiable[S, A]] = Some(this)

    // ---- event behaviour ----

    object changed extends Changed with evt.impl.Generator[S, Update[S, A]] {
      def += (elem: Entry[S, A])(implicit tx: S#Tx): Unit = elem.changed ---> this
      def -= (elem: Entry[S, A])(implicit tx: S#Tx): Unit = elem.changed -/-> this

      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Update[S, A]] = {
        if (pull.isOrigin(this)) return Some(pull.resolve)

        val changes: List[Moved[S, A]] = pull.parents(this).flatMap { evt =>
          val entry = evt.node
          val opt   = pull(evt).map { ch =>
            Moved(ch.asInstanceOf[Change[Long]], entry.asInstanceOf[Entry[S, A]])
          }
          opt
        } (breakOut)

        if (changes.isEmpty) None
        else {
          changes.foreach {
            case Moved(timeChange, entry) =>
              if (timeChange.isSignificant) {
                removeNoFire(timeChange.before, entry)
                addNoFire   (timeChange.now   , entry)
              }
          }
          Some(Update[S, A](pin, changes))
        }
      }
    }

    protected def disposeData()(implicit tx: S#Tx): Unit = {
      tree.iterator.foreach { case (_, xs) =>
          xs.foreach(_.dispose())
      }
      tree.dispose()
    }

    protected def writeData(out: DataOutput): Unit = tree.write(out)

    // ---- collection behaviour ----

    def clear()(implicit tx: S#Tx): Unit = {
      val it = tree.iterator
      if (it.hasNext) {
        val changes = it.toList.flatMap {
          case (timeVal, seq) =>
            seq.map { elem =>
              Removed[S, A](timeVal, elem)
            }
        }
        tree.clear()
        changed.fire(Update(this, changes))
      }
    }

    def add(elem: Entry[S, A])(implicit tx: S#Tx): Unit = {
      val timeVal = elem.key.value // timeValue
      addNoFire(timeVal, elem)

      changed += elem
      changed.fire(Update(pin, Added(timeVal, elem) :: Nil))
    }

    def intersect(time: Long)(implicit tx: S#Tx): Leaf[S, A] = tree.floor(time) match {
      case Some((_, seq)) => seq
      case _ => Vec.empty
    }

    def eventAfter(time: Long)(implicit tx: S#Tx): Option[Long] = tree.ceil(time + 1).map(_._1)

    def at     (time: Long)(implicit tx: S#Tx): Option[Entry[S, A]] = intersect(time).headOption
    def valueAt(time: Long)(implicit tx: S#Tx): Option[A]    = intersect(time).headOption.map(_.value)
    def floor  (time: Long)(implicit tx: S#Tx): Option[Entry[S, A]] = tree.floor(time).flatMap(_._2.headOption)
    def ceil   (time: Long)(implicit tx: S#Tx): Option[Entry[S, A]] = tree.ceil (time).flatMap(_._2.headOption)

    /**
     * Adds a new value, and returns the dirty which corresponds to the new region holding `elem`.
     *
     * @param timeVal the time value at which the new element is inserted
     * @param elem    the element which is inserted
     * @return
     */
    private def addNoFire(timeVal: Long, elem: Entry[S, A])(implicit tx: S#Tx): Unit =
      tree.get(timeVal) match {
        case Some(oldLeaf) =>
          tree += timeVal -> (elem +: oldLeaf)
        case _ =>
          tree += timeVal -> Vec(elem)
      }

    def remove(elem: Entry[S, A])(implicit tx: S#Tx): Boolean = {
      val timeVal = elem.key.value // timeValue
      val (found, visible) = removeNoFire(timeVal, elem)
      if (visible) {
        changed -= elem
        changed.fire(Update(pin, Removed(timeVal, elem) :: Nil))
        elem.dispose()
      }
      found
    }

    private def removeNoFire(timeVal: Long, elem: Entry[S, A])(implicit tx: S#Tx): (Boolean, Boolean) = {
      tree.get(timeVal) match {
        case Some(Vec(single)) =>
          val found = single == elem
          if (found) tree -= timeVal
          (found, found)

        case Some(seq) =>
          val i       = seq.indexOf(elem)
          val found   = i >= 0
          val visible = i == 0
          if (found) {
            val seqNew = seq.patch(i, Vec.empty[Entry[S, A]], 1)
            tree += timeVal -> seqNew
          }
          (found, visible)

        case None => (false, false)
      }
    }

    def debugList()(implicit tx: S#Tx): List[(Long, A)] =
      tree.toList.flatMap {
        case (time, seq) => seq.map(time -> _.value)
      }
  }
}