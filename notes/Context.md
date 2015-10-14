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
