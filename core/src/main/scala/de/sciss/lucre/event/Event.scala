/*
 *  Event.scala
 *  (Lucre)
 *
 *  Copyright (c) 2011-2015 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.event

import de.sciss.lucre.stm.{Disposable, NoSys, Sys}
import de.sciss.serial
import de.sciss.serial.{DataInput, DataOutput}

object Selector {
  implicit def serializer[S <: Sys[S]]: serial.Serializer[S#Tx, S#Acc, Selector[S]] = anySer.asInstanceOf[Ser[S]]

  private val anySer = new Ser[NoSys]

//  private[event] def apply[S <: Sys[S]](slot: Int, node: VirtualNode.Raw[S] /*, invariant: Boolean */): VirtualEvent[S, Any] = {
//    /* if (invariant) */ InvariantTargetsSelector(slot, node)
//    // else MutatingTargetsSelector(slot, node)
//  }

  private[event] def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Selector[S] = {
    val cookie = in.readByte()
    // 0 = invariant, 1 = mutating, 2 = observer
    if (cookie == 0 /* || cookie == 1 */) {
      val slot      = in.readByte() // .readInt()
      // val fullSize  = in.readInt()
      // val reactor   = VirtualNode.read[S](in, fullSize, access)
      val reactor   = Node.read[S](in, access)
      reactor.select(slot /*, cookie == 0 */)
    } else if (cookie == 2) {
      val id = in.readInt()
      new ObserverKey[S](id)
    } else {
      sys.error(s"Unexpected cookie $cookie")
    }
  }

  private final class Ser[S <: Sys[S]] extends serial.Serializer[S#Tx, S#Acc, Selector[S]] {
    def write(v: Selector[S], out: DataOutput): Unit = v.writeSelector(out)

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Selector[S] = Selector.read(in, access)
  }

//  private sealed trait TargetsSelector[S <: Sys[S]] extends VirtualEvent[S, Any] {
//    override private[lucre] def node: VirtualNode.Raw[S]
//
//    final def devirtualize[A, Repr](reader: Reader[S, Repr])(implicit tx: S#Tx): Event[S, A, Repr with Node[S]] =
//      node.devirtualize(reader).select(slot /*, cookie == 0 */)    .asInstanceOf[Event[S, A, Repr with Node[S]]]
//  }
//
//  private final case class InvariantTargetsSelector[S <: Sys[S]](slot: Int, node: VirtualNode.Raw[S])
//    extends TargetsSelector[S] with InvariantSelector[S]
//
//  //  private final case class MutatingTargetsSelector[S <: Sys[S]](slot: Int, node: VirtualNode.Raw[S])
//  //    extends TargetsSelector[S] with MutatingSelector[S]
}

sealed trait Selector[S <: Sys[S]] {
  protected def cookie: Int

  final def writeSelector(out: DataOutput): Unit = {
    out.writeByte(cookie)
    writeSelectorData(out)
  }

  protected def writeSelectorData(out: DataOutput): Unit

  // private[event] def pushUpdate(parent: VirtualEvent[S, Any], push: Push[S]): Unit
  private[event] def pushUpdate(parent: Event[S, Any], push: Push[S]): Unit

  private[event] def toObserverKey: Option[ObserverKey[S]]
}

///** The serializable form of events. */
//sealed trait VirtualNodeSelector[S <: Sys[S]] extends Selector[S] {
//
//  private[lucre] def node: VirtualNode[S]
//
//  private[event] def slot: Int
//
//  final protected def writeSelectorData(out: DataOutput): Unit = {
//    // out.writeInt(slot)
//    out.writeByte(slot)
//    val sizeOffset = out.position
//    out.writeInt(0) // will be overwritten later -- note: addSize cannot be used, because the subsequent write will be invalid!!!
//    node.write(out)
//    val stop = out.position
//    val delta = stop - sizeOffset
//    out.position = sizeOffset
//    val fullSize = delta - 4
//    out.writeInt(fullSize)
//    out.position = stop
//  }
//
//  def devirtualize[A, Repr](reader: Reader[S, Repr])(implicit tx: S#Tx): Event[S, A, Repr with Node[S]]
//
//  override def hashCode: Int = {
//    import MurmurHash3._
//    val h0 = productSeed
//    val h1 = mix(h0, slot)
//    val h2 = mixLast(h1, node.id.##)
//    finalizeHash(h2, 2)
//  }
//
//  override def equals(that: Any): Boolean = that match {
//    case thatSel: VirtualNodeSelector[_] => slot == thatSel.slot && node.id == thatSel.node.id
//    case _ => super.equals(that)
//  }
//
//  final private[event] def toObserverKey: Option[ObserverKey[S]] = None
//
//  override def toString = s"$node.select($slot)"
//}
//
//trait InvariantSelector[S <: Sys[S]] extends VirtualEvent[S, Any] {
//  final protected def cookie: Int = 0
//
//  final private[event] def pushUpdate(parent: VirtualEvent[S, Any], push: Push[S]): Unit =
//    push.visit(this, parent)
//}

/** Instances of `ObserverKey` are provided by methods in `Txn`, when a live `Observer` is registered. Since
  * the observing function is not persisted, the slot will be used for lookup (again through the transaction)
  * of the reacting function during the first reaction gathering phase of event propagation.
  */
final case class ObserverKey[S <: Sys[S]] private[lucre](id: Int) extends /* MMM Expanded */ Selector[S] {
  protected def cookie: Int = 2

  private[event] def toObserverKey: Option[ObserverKey[S]] = Some(this)

//  private[event] def pushUpdate(parent: VirtualEvent[S, Any], push: Push[S]): Unit =
//    push.addLeaf(this, parent)

  private[event] def pushUpdate(parent: Event[S, Any], push: Push[S]): Unit =
    push.addLeaf(this, parent)

  def dispose()(implicit tx: S#Tx) = () // XXX really?

  protected def writeSelectorData(out: DataOutput): Unit = out.writeInt(id)
}

trait EventLike[S <: Sys[S], +A] extends Observable[S#Tx, A] {
  /** Connects the given selector to this event. That is, this event will
    * adds the selector to its propagation targets.
    */
  def ---> (r: Selector[S])(implicit tx: S#Tx): Unit

  /** Disconnects the given selector from this event. That is, this event will
    * remove the selector from its propagation targets.
    */
  def -/-> (r: Selector[S])(implicit tx: S#Tx): Unit

  /** Registers a live observer with this event. The method is called with the
    * observing function which receives the event's update messages, and the
    * method generates an opaque `Disposable` instance, which may be used to
    * remove the observer eventually (through the `dispose` method).
    */
  def react(fun: S#Tx => A => Unit)(implicit tx: S#Tx): Disposable[S#Tx] // Observer[S, A1, Repr]

  /** Called when the first target is connected to the underlying dispatcher node. This allows
    * the event to be lazily added to its sources. A lazy event (most events should be lazy)
    * should call invoke `source ---> this` for each event source. A strict event, an event
    * without sources, or a collection event may simply ignore this method by providing a
    * no-op implementation.
    */
  private[lucre] def connect()(implicit tx: S#Tx): Unit

  /** The counterpart to `connect` -- called when the last target is disconnected from the
    * underlying dispatcher node. Events participating in lazy source registration should use
    * this call to detach themselves from their sources, e.g. call `source -/-> this` for
    * each event source. All other events may ignore this method by providing a
    * no-op implementation.
    */
  private[lucre] def disconnect()(implicit tx: S#Tx): Unit

  /** Involves this event in the pull-phase of event delivery. The event should check
    * the source of the originally fired event, and if it identifies itself with that
    * source, cast the `update` to the appropriate type `A` and wrap it in an instance
    * of `Some`. If this event is not the source, it should invoke `pull` on any
    * appropriate event source feeding this event.
    *
    * @return  the `update` as seen through this event, or `None` if the event did not
    *          originate from this part of the dependency graph or was absorbed by
    *          a filtering function
    */
  private[lucre] def pullUpdate(pull: Pull[S])(implicit tx: S#Tx): Option[A]
}

object Dummy {
  /** This method is cheap. */
  def apply[S <: Sys[S], A]: Dummy[S, A] = anyDummy.asInstanceOf[Dummy[S, A]]

  private val anyDummy = new Impl[NoSys]

  private final class Impl[S <: Sys[S]] extends Dummy[S, Any] {
    override def toString = "event.Dummy"
  }

  private def opNotSupported = sys.error("Operation not supported")
}

trait Dummy[S <: Sys[S], +A] extends EventLike[S, A] {
  import Dummy._

  final def ---> (r: Selector[S])(implicit tx: S#Tx) = ()
  final def -/-> (r: Selector[S])(implicit tx: S#Tx) = ()

  final def react(fun: S#Tx => A => Unit)(implicit tx: S#Tx): Disposable[S#Tx] = Observer.dummy[S]

  final private[lucre] def pullUpdate(pull: Pull[S])(implicit tx: S#Tx): Option[A] = opNotSupported

  final private[lucre] def connect   ()(implicit tx: S#Tx) = ()
  final private[lucre] def disconnect()(implicit tx: S#Tx) = ()
}

/** `Event` is not sealed in order to allow you define traits inheriting from it, while the concrete
  * implementations should extend either of `Event.Constant` or `Event.Node` (which itself is sealed and
  * split into `Event.Invariant` and `Event.Mutating`.
  */
trait Event[S <: Sys[S], +A] extends EventLike[S, A] with Selector[S] /* VirtualEvent[S, Any] */ {
  final protected def cookie: Int = 0
  
  private[event] def slot: Int

  final private[event] def pushUpdate(parent: Event[S, Any], push: Push[S]): Unit =
  push.visit(this, parent)
    
  def node: /* Repr with */ Node[S]

//  final def devirtualize[A1, R1](reader: Reader[S, R1])(implicit tx: S#Tx): Event[S, A1, R1 with Node[S]] =
//    this.asInstanceOf[Event[S, A1, R1 with Node[S]]]

  final def ---> (r: Selector[S])(implicit tx: S#Tx): Unit =
    if (node._targets.add(slot, r)) {
      log(s"$this connect")
      connect()
    }

  final def -/-> (r: Selector[S])(implicit tx: S#Tx): Unit =
    if (node._targets.remove(slot, r)) {
      log(s"$this disconnect")
      disconnect()
    }
}