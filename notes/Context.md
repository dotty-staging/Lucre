    tx.newContext()
    tx.newLongContext()
    
    Context[S <: Sys[S]] extends Disposable[S#Tx] {  // Writable ?
    }
    
    tx.applyIn [A](context: Context*)(v: S#Var[A]): A
    tx.updateIn[A](context: Context*)(v: S#Var[A], x: A): Unit

The crucial point is how events are observed.

    tx.withContext(contexts: _*) {
      expr.changed.react { implicit tx =>
        case Change(_, now) => assert(now == x.value)
      }
    
      // ...
      expr() = x
    }
    
CSS:

- a b    --- b inside a
- a > b  --- b direct child of a
- a + b  --- a and b direct siblings
- a, b   --- union of a and b (must be either a or b)

That doesn't help much. We don't care about nesting,
only side by side OR, AND.

    put (a & b)
      key = Seq(a.key, b.key).sorted.mkString
     
    put (a | b)
      key = a.key
      key = b.key
     
    get (a & b)
      key = Seq(a.key, b.key).sorted.mkString
      key = ()
      
    get (a | b)
      key = a.key
      key = b.key
      key = ()
     
Correct? Then OR would mean the sequence of contexts,
and AND means a refined context.

    trait Context {
      def & (that: Context): Context
      def | (that: Context): Context
    }

---------------------------

Let's look at the more complicated case of a transport context.
This would first of all be

    trait TimeContext[S <: Sys[S]] extends Context[S] {
      def position: Source[S#Tx, Long] with Sink[S#Tx, Long]
    }
    
Since this introduces a mutable element, lot's of things get
complicated: 

- events in for example an `Expr[S, A]`
- do we allow for transitions, e.g. with a `Expr[S, Double]` for a `Curve` element?
- how to provide the rich type, such as `BiPin[S, A]`. when is it created, is it
  lazily stored and how?
- how to deal with collections to give them an equivalent of `BiGroup`?
- how to deal with the reinsertion of the same element at different times?

It feels like we could use a similar approach to `.attrMap`, where that
object is lazily initialised.

We must not shy away from moving things to higher level types, e.g. we can
simply ignore context in `S#Var` and only handle it for `Expr`. Likewise,
we might forget about the idea of transitions and only handle it for
particular types such as `Expr[S, Double]`.

    trait Expr[S <: Sys[S], +A] {
      def inContext(context: S#TimeContext): BiPin[S, Expr[S, A]]
    }
    
The transparency suffers, though...

For the `BiPin`, all we really need is an additional int-slot for the
backing `HASkipList`'s down-node variable, the id-path can be shared.
So ad-hoc construction could be as simply as making a copy of the
base id, and dealing with absent value for the down-node.

On the other hand, we must assume that a timeline will install
listeners on literally all elements, so we'd have to access a
`Targets` for all those virtual bi-pins, so we gain basically
nothing by creating them lazily :-(

The only possible solution here would be that as soon as
the bi-pin becomes necessary, the dependents of the plain
expr (e.g. an attr-map) are copied to the new bi-pin's
targets somehow. ???

Also, looking at `Proc`, basically all expr are values of
the attr-map, so we might as well just replace the
map entries lazily with bi-pins, leaving only `graph`
as a remaining question for upgrade? Ah well, and the `scans`.

Hold a minute---but we already removed 'element-updates'
from the collections, except for special cases such as
the long-expr keys in bi-pin or span-expr keys in bi-group.
(So that was perhaps a smart move, indeed).

An aural view then must still upgrade each object on the
timeline, or more precisely its attr-map. Perhaps there
could be such thing as a creation-event as soon as
the bi-group is created?

    trait Obj[S <: Sys[S]] extends Elem[S] {
      def attrMapCreated: EventLike[S, Unit]
      def contextCreated: EventLike[S, ???]
    }
    
Seems to complicate things on many other levels, but
still looks like a reasonable idea. The object per
definition already has `S#ID`, so if we manage to
also collapse `Targets` to a light-weight thing,
this shouldn't be costly.

Alternatively, the `contextCreated` could be dispatched
on the context itself. Does that make sense? Probably
not, probably that makes maintenance a hell.

-------------------------------------

Regarding the events, perhaps the explicit more
'manual' solution is way easier to grasp. In that case,
a view looking at for example an `Expr[S, A]` will not
be automatically subscribed to the events of any newly
constructed bi-pin. Instead if it interested in tracking
those, it will have to observe the `contextCreated`,
then subscribe to the bi-pin and merge the two
event streams. Which views could we imagine?

- attr-map editor. It could have a switch for enabling
  transport control at all.
- a timeline view. It would be confusing and counter-productive
  if virtual events were fired when the transport advances.
- such virtual events would only ever make sense for real-time
  (momentary) views. let's say we have a `Nuages` view. What
  now? It would make more sense that each view adds to the
  scheduler than to be surprised by some opaque virtual
  events. Especially given that particular views might need
  different timing, such as aural views that pay attention to
  latency.
  
Everything else would be messy. A context would need to
know every object under its control, such that a `position_=`
emits events for those objects. That's simply not possible.

-------------------------------------

How do contexts compose -- do they compose at all?
While we don't need to implement composition from the beginning,
it would be stupid to miss design opportunities for future
addition.

Let's assume, there were still nominal contexts. A proc
is created and placed in a folder. A colour attribute is
added under the folder's context. That proc is brought
onto a timeline. Should there be a way to preserve the
folder-dependent colour on the timeline, and what happens
if the colour is modified in the timeline, depending on
the timeline cursor position?

I don't see a straight forward way to formalise this.
Let's say a context always relates to some outer container
or collection (necessarily an `Obj`).

Let's say two timeline objects are nested. Does it make sense
that the modification of an inner child has anything to do
with the outer timeline's context? It seems that _no_.

In any case, it would be good if we were capable of
querying all the contexts that affected an object.

    trait Obj[S <: Sys[S]] {
      def contexts: Contexts[S]
    }
    
    trait Context[S <: Sys[S]] {
      def owner: Obj[S] // needed?
    }
    
    object Contexts {
      sealed trait Update[S <: Sys[S]]
      case class Added[S <: Sys[S]](context: S#Context) extends Update[S]
    }
    
    trait Contexts[S <: Sys[S]] extends Publisher[S, Contexts.Update[S]] {
      def iterator(implicit tx: S#Tx): Iterator[S#Context]
    }

This creates a lot of space and bidirectional links... Perhaps just
query with a context:

    trait Expr[S <: Sys[S], A] {
      object contexts extends Publisher[S, ContextUpdate[S]] {
        def get(context: S#Context)(implicit tx: S#Tx): Option[BiPin[S, Expr[S, A]]]
      }
    }

That looks awful and wrong :(

What kind of objects have we got?

- expr-like. For a `TimeContext` we'd get a bi-pin (or something refined for `Expr[S, Double]`)
- collections. E.g. `Folder`, `Timeline`, `Scan`. For a `TimeContext` we'd get a bi-group
- composites. We simply look at all elements individually, e.g. `graph`. `inputs` and `outputs` for `Proc`

-------------------------------------

Of course, we could simplify life a lot but just giving up on the general context
idea (for now), and just adding bi-pin kind of functionality where it would be
needed to fully reflect a nuages-session in the timeline-view:

- `Proc.graph`
- `Scan`

That implicates that no ports can be added or removed in performance time, neither can
the name be changed, fade curves etc. We might add the possibility to "automate" booleans
such as "mute". This removes static typing, because then `graph` would have to essentially
become a `S#Var[Obj[S]]` just like the values of the attribute map :( Then it would even
make sense to remove `graph` altogether and provide a conventional attr-map key. Or not
allow graph modification in tP (fair enough). That leaves the question of the scans...

    sealed trait ScanTarget
    trait ListScanTarget     extends ScanTarget with List   [S, Scan]
    trait TimelineScanTarget extends ScanTarget with BiGroup[S, Scan]
    
    trait Scan {
      def target: ScanTarget
      def target_=(t: ScanTarget): Unit
    }
    
? _Not pretty_

Of course, if like in a _graph_, the scan-links like _edges_ were part of the parent
structure and not the _vertices_ aka procs, then this might be simpler.

    // before
    timeline.add(span, proc)
    proc.outputs.add("out").add(Scan.Link.Scan(target))
    
    // after
    timeline.add(span1, proc)
    val scan = proc.outputs.add("out")
    val link = Scan.Link.Scan(target)
    timeline.add(span2, link)

And

    // before
    ensemble.folder.addLast(proc)
    proc.outputs.add("out").add(Scan.Link.Scan(target))
    
    // after
    ensemble.folder.addLast(proc)
    val scan = proc.outputs.add("out")
    val link = Scan.Link.Scan(target)
    ensemble.folder.addLast(link)

? Seems straight forward.

What happens with the awful mapping of scans to attribute values? Actually wasn't that
even part of the grapheme interface?

We should just remove `inputs` altogether and simply rebuild them as a cache of
strings from the graph. Then the links always go into the attribute map.

-------------------------------------

The disadvantage of not having context is that having a bi-pin
value somewhere doesn't make sense if the context is not a timeline.

-------------------------------------

The `copy` method would also need to support contexts.