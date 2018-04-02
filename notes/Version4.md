# Replacing `S < Sys[S]` by `T <: Txn`

Reasons:

- abstract type projections will go away in Dotty
- we want to type less and thus get rid of the F-bounded type
- we want macros for easier serializer generation
  (thus we want to rethink serializers)
  
## `Acc`

So we managed to switch the positions of `Tx` and `Acc` in serialization, and
make `Acc` go away as a constructor type parameter.

## `Var` and serializers

This seems to be mostly working as well, since once you've got the variable, you
may treat it as generic `stm.Var`, and there's no need to designate it as
`tx.Var`. The problem is the serializer for generic `newVar`:

    def newVar[A](id: Id, init: A)(implicit serializer: Serializer[?, A]): Var[A]

In general, it would be great if we could remove `T` from `Serializer` at all, moving
it instead to the `read` method. That would imply that any auxiliary structures required
for the serializer would also change their API from being constructively typed in `T` to
using type parameters on their methods instead. This might actually be doable—however,
there is a remaining problem which is the requirement for having a sub-system type,
certainly for `lucre.synth.Sys` or in some cases perhaps a confluent system? So the
question is if we can express that while keeping the `T` type on `read`? (Because, let's
face it, almost all serializers now do the dirty `anySer.asInstanceOf` trick to avoid
instantiation).

Well, another problem would be the `T` parameter of the payload, like

    trait FooSer[S] extends Serializer[S#Tx, S#Acc, Foo[S]] {
      def write(foo: Foo[S] /* ! */, out: DataOutput): Unit
      def read(in: DataInput, acc: S#Acc)(implicit tx: S#Tx): Foo[S] /* ! */
    }

How could that look?

    trait Serializer[A[_ <: Txn]] {
      type A1[T <: Txn] = A[T]
    
      def write[T <: Txn](value: A1[T], out: DataOutput): Unit
      def read[T <: Txn](in: DataInput, tx: T)(implicit access: tx.Acc): A1[T]
    }

    trait Foo[T <: Txn]

    trait FooSer extends Serializer[Foo] {
      def write[T <: Txn](foo: Foo[T], out: DataOutput): Unit
      def read[T <: Txn](in: DataInput, tx: T)(implicit access: tx.Acc): Foo[T]
    }

What would happen for non-transactional values?

    object ImmutableSerializer {
      type NonTxn[A]
    }
    trait ImmutableSerializer[B] extends Serializer[({type A[_ <: Txn] = B})#A] {
      def write[T <: Txn](value: A1[T], out: DataOutput): Unit
      def read [T <: Txn](in: DataInput, tx: T)(implicit access: tx.Acc): A1[T]
    }
    
Can we do without the type-lambda?

In any case, __this won't work,__ we have nested serializers, like so:

    object markSerializer extends Serializer[S#Tx, S#Acc, M] {
      def write(v: M, out: DataOutput): Unit = v.write(out)
    
      def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): M = new M {
        def map: MapImpl[S, Version, A] = me /* !!! */
        ...
      }
    }
    
So this is tight to an early binding of `S`. (This is a repetition of the `Action.Universe`
"problem").

Go back:

    trait Txn {
      type Id
      type Var[A]
    
      def newVar[A](id: Id, init: A)(implicit serializer: Serializer[?, A]): Var[A]
    }
    
What happens if we add a type argument?

    def newVar[A, T <: Txn](id: Id, init: A)(implicit serializer: Serializer[T, A]): Var[A]

This won't make sense, because the receiving txn can't do anything with the serializer.

    object Sys {
      def newVar[A, T <: Txn](tx: T)(id: tx.Id, init: A)
                             (implicit serializer: Serializer[T, A]): tx.Var[A] = 
        tx.newVar[A](id, init)
    }

Fuck me, that compiles. I still suspect that `this.type` will cause a problem for later
deserialization. We really need a `Repr`.