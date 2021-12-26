# Event bus

An attempt to retroactively document how they work in the current (4.5.x) version. A brief overview is found in
the paper 'A Reactive, Confluently Persistent Framework for the Design of Computer Music Systems', now to a degree
superseded after the change from `S#Tx` to `T`.

## Event

As a supertype, `trait EventLike[T <: Txn[T], +A]` can represent both dummy and real events. From `Observable` it
inherits the live view registration `def react(fun: Tx => A => Unit)(implicit tx: Tx): Disposable[Tx]`. Dependent
(sink) events can register and unregister via `--->` and `-/->`. The event like's update producing function is
`def pullUpdate(pull: Pull[T])(implicit tx: T): Option[A]`. The actual event extends the event like as
`trait Event[T <: Txn[T], +A] extends EventLike[T, A] with Writable`, basically adding final implementations for
`--->`, `-/->`, `react`, where the latter just calls `Observer(this, fun)`, and the former two rely on the new
methods `def node: Event.Node[T]` and `def slot: Int`.

## Event.Node

An `Event.Node` extends the `Elem` interface, implying that it part of a globally registered set of elements which can
thus  be serialised without needing to hold a specific serialiser. From `Elem`, the node inherits the method
`def event(slot: Int): Event[T, Any]` as well as `def changed: EventLike[T, A]`, where `A` is `Any` and may be refined
by the node. The main additions of a node are method `def targets: Targets[T]` and `def id: Ident[T]` which is based on
the targets. Many nodes (objects) only emit a single event, and can thus use the implementation trait `SingleEventNode`.

## Event.Targets

An event targets instances collects the dependencies for a node, in the form of methods
`def add(slot: Int, sel: Event[T, Any])(implicit tx: T): Boolean` and
`def remove(slot: Int, sel: Event[T, Any])(implicit tx: T): Boolean`. Here, the slot refers to the emitting event,
part of the node which holds the targets. The boolean result indicates whether the registration is the _first_ one,
and whether the de-registration is the _last_ one. Since `add` and `remove` are called by the final implementations
of `--->` and `-/->` in event, these return states are currently not used.

__Note:__ At this point, it seems to me that we no longer distinguish "normal" and "event" variables, and thus
any object that has ever been connected to another will keep that other object in its targets children, even if
the sink has since been removed from the workspace (as we essentially never call `dispose`)? The removal of
event variables happened on 19 August 2015.

## Push, Pull

