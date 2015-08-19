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
    