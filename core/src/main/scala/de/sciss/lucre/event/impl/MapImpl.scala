/*
 *  MapImpl.scala
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

package de.sciss.lucre.event
package impl

import de.sciss.lucre.data.{Ordering, SkipList}
import de.sciss.lucre.event.Map.{Key, Modifiable}
import de.sciss.lucre.stm.impl.ObjSerializer
import de.sciss.lucre.stm.{Copy, Elem, Obj, Sys}
import de.sciss.lucre.{event => evt}
import de.sciss.serial.{DataInput, DataOutput, Serializer}

import scala.language.higherKinds
import scala.reflect.ClassTag

object MapImpl {
  def apply[S <: Sys[S], K, Repr[~ <: Sys[~]] <: Elem[~]](implicit tx: S#Tx, keyType: Key[K]): Modifiable[S, K, Repr] = {
    val targets = evt.Targets[S]
    new Impl[S, K, Repr](targets) {
      val peer = SkipList.Map.empty[S, K, List[Entry[K, V]]](tx, keyOrdering, keyType.serializer,
        Serializer.list(entrySerializer))
    }
  }

  def serializer[S <: Sys[S], K, Repr[~ <: Sys[~]] <: Elem[~]](implicit keyType: Key[K]): Serializer[S#Tx, S#Acc, Map[S, K, Repr]] =
    new Ser[S, K, Repr]

  def modSerializer[S <: Sys[S], K, Repr[~ <: Sys[~]] <: Elem[~]](implicit keyType: Key[K]): Serializer[S#Tx, S#Acc, Modifiable[S, K, Repr]] =
    new ModSer[S, K, Repr]

  private class Ser[S <: Sys[S], K, Repr[~ <: Sys[~]] <: Elem[~]](implicit keyType: Key[K])
    extends ObjSerializer[S, Map[S, K, Repr]] {

    def tpe = Map
  }

  private class ModSer[S <: Sys[S], K, Repr[~ <: Sys[~]] <: Elem[~]](implicit keyType: Key[K])
    extends ObjSerializer[S, Modifiable[S, K, Repr]] {

    def tpe = Map
  }

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] = {
    val targets   = evt.Targets.read(in, access)
    val keyTypeID = in.readInt()
    val keyType   = Key(keyTypeID) // Obj.getType(keyTypeID).asInstanceOf[Key[_]]
    mkRead(in, access, targets)(tx, keyType)
  }

  private def mkRead[S <: Sys[S], K, Repr[~ <: Sys[~]] <: Elem[~]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                               (implicit tx: S#Tx, keyType: Key[K]): Impl[S, K, Repr] =
    new Impl[S, K, Repr](targets) {
      val peer = SkipList.Map.read[S, K, List[Entry[K, V]]](in, access)(tx, keyOrdering, keyType.serializer,
        Serializer.list(entrySerializer))
    }

  private final class Entry[K, V](val key: K, val value: V)
 
  private abstract class Impl[S <: Sys[S], K, Repr[~ <: Sys[~]] <: Elem[~]](protected val targets: evt.Targets[S])
                                                          (implicit val keyType: Key[K])
    extends Modifiable[S, K, Repr] with evt.impl.SingleNode[S, Map.Update[S, K, Repr]] {
    map =>

    final def tpe: Obj.Type = Map

    // ---- abstract ----

    protected def peer: SkipList.Map[S, K, List[Entry[K, V]]]
    
    // ---- implemented ----

    private[lucre] def copy()(implicit tx: S#Tx, copy: Copy[S]): Elem[S] = {
      val res = Map.Modifiable[S, K, Elem /* Repr */]
      iterator.foreach { case (k, v) =>
        res.put(k, copy(v))
      }
      res
    }

    implicit object keyOrdering extends Ordering[S#Tx, K] {
      def compare(a: K, b: K)(implicit tx: S#Tx): Int = {
        val ah = a.hashCode() // ##
        val bh = b.hashCode() // ##
        if (ah < bh) -1 else if (ah > bh) 1 else 0
      }
    }

    object entrySerializer extends Serializer[S#Tx, S#Acc, Entry[K, V]] {
      def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Entry[K, V] = {
        val key   = keyType.serializer.read(in)
        val value = Elem.read(in, access).asInstanceOf[V]
        new Entry[K, V](key, value)
      }

      def write(entry: Entry[K, V], out: DataOutput): Unit = {
        keyType.serializer.write(entry.key, out)
        entry.value.write(out)
      }
    }

    final def contains(key: K)(implicit tx: S#Tx): Boolean    = peer.get(key).exists(vec => vec.exists(_.key == key))
    final def get     (key: K)(implicit tx: S#Tx): Option[V]  = peer.get(key).flatMap { vec =>
      vec.collectFirst {
        case entry if entry.key == key => entry.value
      }
    }

    final def iterator(implicit tx: S#Tx): Iterator[(K, V)] = peer.iterator.flatMap {
      case (key, vec) => vec.map(entry => key -> entry.value)
    }
    final def keysIterator  (implicit tx: S#Tx): Iterator[K] = peer.valuesIterator.flatMap(_.map(_.key  ))
    final def valuesIterator(implicit tx: S#Tx): Iterator[V] = peer.valuesIterator.flatMap(_.map(_.value))

    final def $[R[~ <: Sys[~]] <: Repr[~]](key: K)(implicit tx: S#Tx, ct: ClassTag[R[S]]): Option[R[S]] =
      peer.get(key).flatMap { vec =>
        vec.collectFirst {
          case entry if entry.key == key && ct.runtimeClass.isAssignableFrom(entry.value.getClass) =>
            entry.value.asInstanceOf[R[S]]
        }
      }

    final def size(implicit tx: S#Tx): Int = {
      // XXX TODO: a bit ugly...
      var res = 0
      peer.valuesIterator.foreach(res += _.size)
      res
    }
    final def nonEmpty(implicit tx: S#Tx): Boolean  = peer.nonEmpty
    final def isEmpty (implicit tx: S#Tx): Boolean  = peer.isEmpty

    final def modifiableOption: Option[Modifiable[S, K, Repr]] = Some(this)

    final protected def writeData(out: DataOutput): Unit = {
      out.writeInt(keyType.typeID)
      peer.write(out)
    }

    final protected def disposeData()(implicit tx: S#Tx): Unit = peer.dispose()

    override def toString = s"Map$id"
    
    final protected def foreach(fun: Entry[K, V] => Unit)(implicit tx: S#Tx): Unit =
      peer.valuesIterator.foreach(_.foreach(fun))

    object changed extends Changed
      with evt.impl.Generator[S, Map.Update[S, K, Repr]] with evt.impl.Root[S, Map.Update[S, K, Repr]]

    private[this] def fireAdded(key: K, value: V)(implicit tx: S#Tx): Unit =
      changed.fire(Map.Update(map, Map.Added[S, K, V](key, value) :: Nil))

    private[this] def fireRemoved(key: K, value: V)(implicit tx: S#Tx): Unit =
      changed.fire(Map.Update(map, Map.Removed[S, K, V](key, value) :: Nil))

    final def +=(kv: (K, V))(implicit tx: S#Tx): this.type = {
      put(kv._1, kv._2)
      this
    }

    final def -=(key: K)(implicit tx: S#Tx): this.type = {
      remove(key)
      this
    }

    final def put(key: K, value: V)(implicit tx: S#Tx): Option[V] = {
      val entry     = new Entry[K, V](key, value)
      val oldVec    = peer.get(key).getOrElse(Nil)
      val idx       = oldVec.indexWhere(_.key == key)
      val found     = idx >= 0
      val newVec    = if (found) oldVec.updated(idx, entry) else oldVec :+ entry
      peer.add(key -> newVec)

      if (found) {
        val oldEntry = oldVec(idx)
        // unregisterElement(oldEntry)
        fireRemoved(key, oldEntry.value)
      }
      // registerElement(entry)
      fireAdded(key, value)

      if (found) Some(oldVec(idx).value) else None
    }

    final def remove(key: K)(implicit tx: S#Tx): Option[V] = {
      val oldVec  = peer.get(key).getOrElse(Nil)
      val idx     = oldVec.indexWhere(_.key == key)
      if (idx < 0) return None

      val entry   = oldVec(idx)
      val value   = entry.value
      val newVec  = oldVec.patch(idx, Nil, 1)
      if (newVec.isEmpty) {
        peer.remove(key)
      } else {
        peer.add(key -> newVec)
      }

      // unregisterElement(entry)
      fireRemoved(key, value)

      Some(value)
    }
  }
}