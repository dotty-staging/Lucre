/*
 *  ListImpl.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2016 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr
package impl

import de.sciss.lucre.event.{Targets, impl => eimpl}
import de.sciss.lucre.stm.impl.ObjSerializer
import de.sciss.lucre.stm.{Copy, Elem, NoSys, Obj, Sys}
import de.sciss.lucre.{event => evt}
import de.sciss.serial.{DataInput, DataOutput, Serializer}

import scala.annotation.{switch, tailrec}
import scala.language.higherKinds

object ListImpl {
  import de.sciss.lucre.expr.List.Modifiable

  def newModifiable[S <: Sys[S], E[~ <: Sys[~]] <: Elem[~]](implicit tx: S#Tx): Modifiable[S, E[S]] =
    new Impl1[S, E] {
      protected val targets: Targets[S] = evt.Targets[S]
      protected val sizeRef: S#Var[Int] = tx.newIntVar(id, 0)
      protected val headRef: S#Var[C]   = tx.newVar[C](id, null)(CellSer)
      protected val lastRef: S#Var[C]   = tx.newVar[C](id, null)(CellSer)
    }

  def serializer[S <: Sys[S], A <: Elem[S]]: Serializer[S#Tx, S#Acc, List[S, A]] =
    anySer.asInstanceOf[Ser[S, A]]

  private val anySer = new Ser[NoSys, Obj[NoSys]]

  def modSerializer[S <: Sys[S], A <: Elem[S]]: Serializer[S#Tx, S#Acc, Modifiable[S, A]] =
    anyModSer.asInstanceOf[ModSer[S, A]]

  private val anyModSer = new ModSer[NoSys, Obj[NoSys]]

  private class Ser[S <: Sys[S], A <: Elem[S]] extends ObjSerializer[S, List[S, A]] {
    def tpe = List
  }

  private class ModSer[S <: Sys[S], A <: Elem[S]] extends ObjSerializer[S, Modifiable[S, A]] {
    def tpe = List
  }

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] = {
    val targets = Targets.read[S](in, access)
    ListImpl.read(in, access, targets)
  }

  private def read[S <: Sys[S], E[~ <: Sys[~]] <: Elem[~]](in: DataInput, access: S#Acc, _targets: evt.Targets[S])
                                               (implicit tx: S#Tx): Impl[S, E] =
    new Impl1[S, E] {
      protected val targets: Targets[S] = _targets
      protected val sizeRef: S#Var[Int] = tx.readIntVar(id, in)
      protected val headRef: S#Var[C]   = tx.readVar[C](id, in)
      protected val lastRef: S#Var[C]   = tx.readVar[C](id, in)
    }

  final class Cell[S <: Sys[S], A](val elem: A,
                                   val pred: S#Var[Cell[S, A]],
                                   val succ: S#Var[Cell[S, A]])

  private final class Iter[S <: Sys[S], A](private var cell: Cell[S, A])(implicit tx: S#Tx) extends Iterator[A] {
    override def toString: String = if (cell == null) "empty iterator" else "non-empty iterator"

    def hasNext: Boolean = cell != null

    def next(): A = {
      if (cell == null) throw new NoSuchElementException("next on empty iterator")
      val res = cell.elem
      cell    = cell.succ()
      res
    }
  }

  private def copyList[In <: Sys[In], Out <: Sys[Out], E[~ <: Sys[~]] <: Elem[~]](in : List.Modifiable[In , E[In ]],
                                                                                  out: List.Modifiable[Out, E[Out]])
                                                                                 (implicit txIn: In#Tx, txOut: Out#Tx,
                                                                                  context: Copy[In, Out]): Unit = {
    in.iterator.foreach { elem =>
      out.addLast(context(elem))
    }
  }

  abstract class Impl[S <: Sys[S], E[~ <: Sys[~]] <: Elem[~]]
    extends Modifiable[S, E[S]] with eimpl.SingleNode[S, List.Update[S, E[S]]] { list =>

    type A = E[S]
    protected type ListAux[~ <: Sys[~]] = List[~, E[~]]

    final protected type C = Cell[S, A]

    protected def headRef: S#Var[C]
    protected def lastRef: S#Var[C]
    protected def sizeRef: S#Var[Int]

    // ---- event behaviour ----

    protected implicit object CellSer extends Serializer[S#Tx, S#Acc, C] {
      def write(cell: C, out: DataOutput): Unit =
        if (cell != null) {
          out.writeByte(1)
          cell.elem.write(out)
          cell.pred.write(out)
          cell.succ.write(out)
        } else {
          out.writeByte(0)
        }

      def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): C =
        (in.readByte: @switch) match {
          case 1 =>
            val elem = Elem.read(in, access).asInstanceOf[A]
            val pred = tx.readVar[C](id, in)
            val succ = tx.readVar[C](id, in)
            new Cell[S, A](elem, pred, succ)
          case 0 => null
          case cookie => sys.error(s"Unexpected cookie $cookie")
        }
    }

    // protected def reader: evt.Reader[S, List[S, A, U]]

    object changed extends Changed
      with eimpl.Generator[S, List.Update[S, A]]
      with eimpl.Root[S, List.Update[S, A]]

    final def indexOf(elem: A)(implicit tx: S#Tx): Int = {
      var idx = 0
      var rec = headRef()
      while (rec != null) {
        if (rec.elem == elem) return idx
        idx += 1
        rec = rec.succ()
      }
      -1
    }

    final def apply(idx: Int)(implicit tx: S#Tx): A =
      get(idx).getOrElse(throw new IndexOutOfBoundsException(idx.toString))

    final def get(idx: Int)(implicit tx: S#Tx): Option[A] = {
      if (idx < 0) return None
      var left = idx
      var rec = headRef()
      while (rec != null && left > 0) {
        left -= 1
        rec = rec.succ()
      }
      if (rec == null) None else Some(rec.elem)
    }

    final def addLast(elem: A)(implicit tx: S#Tx): Unit = {
      val pred      = lastRef()
      val succ      = null
      val idx       = sizeRef()
      insert(elem, pred, succ, idx)
    }

    final def addHead(elem: A)(implicit tx: S#Tx): Unit = {
      val pred      = null
      val succ      = headRef()
      val idx       = 0
      insert(elem, pred, succ, idx)
    }

    def insert(index: Int, elem: A)(implicit tx: S#Tx): Unit = {
      if (index < 0)      throw new IndexOutOfBoundsException(index.toString)
      var pred      = null: C
      var succ      = headRef()
      var idx       = 0
      while (idx < index) {
        if (succ == null) throw new IndexOutOfBoundsException(index.toString)
        pred  = succ
        succ  = succ.succ()
        idx  += 1
      }
      insert(elem, pred, succ, idx)
    }

    private[this] def insert(elem: A, pred: C, succ: C, idx: Int)(implicit tx: S#Tx): Unit = {
      val recPred   = tx.newVar[C](id, pred)
      val recSucc   = tx.newVar[C](id, succ)
      val rec       = new Cell[S, A](elem, recPred, recSucc)
      val predSucc  = if (pred == null) headRef else pred.succ
      val succPred  = if (succ == null) lastRef else succ.pred
      predSucc()    = rec
      succPred()    = rec
      sizeRef()     = sizeRef() + 1

      fireAdded(idx, elem)
    }

    final protected def foreach(fun: A => Unit)(implicit tx: S#Tx): Unit = {
      @tailrec def loop(cell: C): Unit =
        if (cell != null) {
          fun(cell.elem)
          loop(cell.succ())
        }

      loop(headRef())
    }

    private[this] def fireAdded(idx: Int, elem: A)(implicit tx: S#Tx): Unit =
      changed.fire(List.Update(list, Vector(List.Added(idx, elem))))

    private[this] def fireRemoved(idx: Int, elem: A)(implicit tx: S#Tx): Unit =
      changed.fire(List.Update(list, Vector(List.Removed(idx, elem))))

    final def remove(elem: A)(implicit tx: S#Tx): Boolean = {
      var rec = headRef()
      var idx = 0
      while (rec != null) {
        if (rec.elem == elem) {
          removeCell(rec)
          fireRemoved(idx, elem)
          return true
        }
        rec = rec.succ()
        idx += 1
      }
      false
    }

    final def removeAt(index: Int)(implicit tx: S#Tx): A = {
      if (index < 0) throw new IndexOutOfBoundsException(index.toString)
      var rec = headRef()
      if (rec == null) throw new IndexOutOfBoundsException(index.toString)
      var idx = 0
      while (idx < index) {
        rec = rec.succ()
        if (rec == null) throw new IndexOutOfBoundsException(index.toString)
        idx += 1
      }

      val e = rec.elem
      removeCell(rec)
      fireRemoved(idx, e)
      e
    }

    // unlinks a cell and disposes it. does not fire. decrements sizeRef
    private[this] def removeCell(cell: C)(implicit tx: S#Tx): Unit = {
      val pred = cell.pred()
      val succ = cell.succ()
      if (pred != null) {
        pred.succ() = succ
      } else {
        headRef() = succ
      }
      if (succ != null) {
        succ.pred() = pred
      } else {
        lastRef() = pred
      }
      sizeRef() = sizeRef() - 1 // .transform(_ - 1)
      disposeCell(cell)
    }

    final def removeLast()(implicit tx: S#Tx): A = {
      val rec = lastRef()
      if (rec == null) throw new NoSuchElementException("last of empty list")

      val pred  = rec.pred()
      val e     = rec.elem
      val idx   = sizeRef() - 1
      disposeCell(rec)
      sizeRef() = idx
      lastRef() = pred
      if (pred == null) {
        headRef() = null
      } else {
        pred.succ() = null
      }
      fireRemoved(idx, e)
      e
    }

    final def removeHead()(implicit tx: S#Tx): A = {
      val rec = headRef()
      if (rec == null) throw new NoSuchElementException("head of empty list")

      val succ = rec.succ()
      val e = rec.elem
      disposeCell(rec)
      sizeRef() = sizeRef() - 1 // .transform(_ - 1)
      headRef() = succ
      if (succ == null) {
        lastRef() = null
      } else {
        succ.pred() = null
      }
      fireRemoved(0, e)
      e
    }

    final def clear()(implicit tx: S#Tx): Unit =
      while (nonEmpty) removeLast()

    // deregisters element event. disposes cell contents, but does not unlink, nor fire.
    private[this] def disposeCell(cell: C)(implicit tx: S#Tx): Unit = {
      // unregisterAent(cell.elem)
      cell.pred.dispose()
      cell.succ.dispose()
    }

    final protected def disposeData()(implicit tx: S#Tx): Unit = {
      var rec = headRef()
      while (rec != null) {
        val tmp = rec.succ()
        disposeCell(rec)
        rec = tmp
      }
      sizeRef.dispose()
      headRef.dispose()
      lastRef.dispose()
    }

    final protected def writeData(out: DataOutput): Unit = {
      sizeRef.write(out)
      headRef.write(out)
      lastRef.write(out)
    }

    final def isEmpty (implicit tx: S#Tx): Boolean = size == 0
    final def nonEmpty(implicit tx: S#Tx): Boolean = size > 0
    final def size    (implicit tx: S#Tx): Int     = sizeRef()

    final def headOption(implicit tx: S#Tx): Option[A] = {
      val rec = headRef()
      if (rec != null) Some(rec.elem) else None
    }

    final def lastOption(implicit tx: S#Tx): Option[A] = {
      val rec = lastRef()
      if (rec != null) Some(rec.elem) else None
    }

    final def head(implicit tx: S#Tx): A = {
      val rec = headRef()
      if (rec != null) rec.elem else throw new NoSuchElementException("head of empty list")
    }

    final def last(implicit tx: S#Tx): A = {
      val rec = lastRef()
      if (rec != null) rec.elem else throw new NoSuchElementException("last of empty list")
    }

    final def iterator(implicit tx: S#Tx): Iterator[A] = new Iter(headRef())
  }

  private abstract class Impl1[S <: Sys[S], E[~ <: Sys[~]] <: Elem[~]] extends Impl[S, E] {
    in =>

    final def tpe: Obj.Type = List

    override def toString = s"List$id"

    def modifiableOption: Option[List.Modifiable[S, A]] = Some(this)

    final private[lucre] def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx,
                                                     context: Copy[S, Out]): Elem[Out] = {
      val out = newModifiable[Out, E]
      context.defer[ListAux](in, out)(copyList(in, out))
      // .connect
      out
    }
  }
}