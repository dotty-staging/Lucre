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
import de.sciss.serial.{DataInput, DataOutput, Writable}

trait EventLike[S <: Sys[S], +A] extends Observable[S#Tx, A] {
  /** Connects the given selector to this event. That is, this event will
    * adds the selector to its propagation targets.
    */
  def ---> (sink: Event[S, Any])(implicit tx: S#Tx): Unit

  /** Disconnects the given selector from this event. That is, this event will
    * remove the selector from its propagation targets.
    */
  def -/-> (sink: Event[S, Any])(implicit tx: S#Tx): Unit

  /** Registers a live observer with this event. The method is called with the
    * observing function which receives the event's update messages, and the
    * method generates an opaque `Disposable` instance, which may be used to
    * remove the observer eventually (through the `dispose` method).
    */
  def react(fun: S#Tx => A => Unit)(implicit tx: S#Tx): Disposable[S#Tx]

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

  final def ---> (sink: Event[S, Any])(implicit tx: S#Tx) = ()
  final def -/-> (sink: Event[S, Any])(implicit tx: S#Tx) = ()

  final def react(fun: S#Tx => A => Unit)(implicit tx: S#Tx): Disposable[S#Tx] = Observer.dummy[S]

  final private[lucre] def pullUpdate(pull: Pull[S])(implicit tx: S#Tx): Option[A] = opNotSupported
}

object Event {
  implicit def serializer[S <: Sys[S]]: serial.Serializer[S#Tx, S#Acc, Event[S, Any]] = anySer.asInstanceOf[Ser[S]]

  private val anySer = new Ser[NoSys]

  private[event] def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Event[S, Any] = {
    val slot  = in.readByte()
    val node  = Node.read[S](in, access)
    node.select(slot)
  }

  private final class Ser[S <: Sys[S]] extends serial.Serializer[S#Tx, S#Acc, Event[S, Any]] {
    def write(e: Event[S, Any], out: DataOutput): Unit = e.write(out)
    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Event[S, Any] = Event.read(in, access)
  }
}

/** `Event` is not sealed in order to allow you define traits inheriting from it, while the concrete
  * implementations should extend either of `Event.Constant` or `Event.Node` (which itself is sealed and
  * split into `Event.Invariant` and `Event.Mutating`.
  */
trait Event[S <: Sys[S], +A] extends EventLike[S, A] with Writable {
  // ---- abstract ----

  def node: Node[S]

  private[event] def slot: Int

  // ---- implemented ----

  final private[event] def pushUpdate(parent: Event[S, Any], push: Push[S]): Unit =
    push.visit(this, parent)
    

  final def ---> (sink: Event[S, Any])(implicit tx: S#Tx): Unit = {
    node._targets.add(slot, sink)
//    if (node._targets.add(slot, r) && !tx.reactionMap.hasEventReactions(this)) {
//      log(s"$this connect")
//      connect()
//    }
  }

  final def -/-> (sink: Event[S, Any])(implicit tx: S#Tx): Unit = {
    node._targets.remove(slot, sink)
//    if (node._targets.remove(slot, r) && !tx.reactionMap.hasEventReactions(this)) {
//      log(s"$this disconnect")
//      disconnect()
//    }
  }

  final def write(out: DataOutput): Unit = {
    out.writeByte(slot)
    node.write(out)
  }

  final def react(fun: S#Tx => A => Unit)(implicit tx: S#Tx): Disposable[S#Tx] = Observer(this, fun)
}