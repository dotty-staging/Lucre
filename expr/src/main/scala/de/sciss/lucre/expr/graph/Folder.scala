/*
 *  Folder.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2019 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr.graph

import de.sciss.lucre.adjunct.{Adjunct, ProductWithAdjuncts}
import de.sciss.lucre.edit.EditFolder
import de.sciss.lucre.event.impl.IChangeGenerator
import de.sciss.lucre.event.{Caching, IChangeEvent, IPull, IPush, ITargets}
import de.sciss.lucre.expr.graph.impl.{AbstractCtxCellView, ExpandedObjMakeImpl, ObjCellViewVarImpl, ObjImplBase}
import de.sciss.lucre.expr.impl.IActionImpl
import de.sciss.lucre.expr.{CellView, Context, IAction, IExpr}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.stm.{Disposable, Sys}
import de.sciss.model.Change
import de.sciss.serial.{DataInput, Serializer}

import scala.concurrent.stm.Ref

object Folder {
  private lazy val _init: Unit =
    Adjunct.addFactory(Bridge)

  def init(): Unit = _init

  def apply(): Ex[Folder] with Obj.Make = Apply()

  private[lucre] object Empty extends Folder {
    private[lucre] def peer[S <: Sys[S]](implicit tx: S#Tx): Option[Peer[S]] = None

    override def toString: String = "Folder<empty>"
  }

  private final class ApplyExpanded[S <: Sys[S]](implicit targets: ITargets[S])
    extends ExpandedObjMakeImpl[S, Folder] {

    protected def empty: Folder = Empty

    protected def make()(implicit tx: S#Tx): Folder = {
      val peer = stm.Folder[S]()
      new Impl(tx.newHandle(peer), tx.system)
    }
  }

  private final case class Apply() extends Ex[Folder] with Act with Obj.Make {
    override def productPrefix: String = "Folder" // serialization

    type Repr[S <: Sys[S]] = IExpr[S, Folder] with IAction[S]

    def make: Act = this

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new ApplyExpanded[S]
    }
  }

  private[lucre] def wrapH[S <: Sys[S]](peer: stm.Source[S#Tx, stm.Folder[S]], system: S): Folder =
    new Impl[S](peer, system)

  private[lucre] def wrap[S <: Sys[S]](peer: stm.Folder[S])(implicit tx: S#Tx): Folder =
    new Impl[S](tx.newHandle(peer), tx.system)

  private final class Impl[S <: Sys[S]](in: stm.Source[S#Tx, stm.Folder[S]], system: S)
    extends ObjImplBase[S, stm.Folder](in, system) with Folder {

    override type Peer[~ <: Sys[~]] = stm.Folder[~]

    override def toString: String = s"Folder($in)"
  }

  private final class CellViewImpl[S <: Sys[S]](h: stm.Source[S#Tx, stm.Obj[S]], key: String)
    extends ObjCellViewVarImpl[S, stm.Folder, Folder](h, key) {

    protected def lower(peer: stm.Folder[S])(implicit tx: S#Tx): Folder =
      wrap(peer)

    implicit def serializer: Serializer[S#Tx, S#Acc, Option[stm.Folder[S]]] =
      Serializer.option
  }

  implicit object Bridge extends Obj.Bridge[Folder] with Adjunct.Factory {
    final val id = 2001

    type Repr[S <: Sys[S]] = stm.Folder[S]

    def readIdentifiedAdjunct(in: DataInput): Adjunct = this

    def cellView[S <: Sys[S]](obj: stm.Obj[S], key: String)(implicit tx: S#Tx): CellView.Var[S#Tx, Option[Folder]] =
      new CellViewImpl(tx.newHandle(obj), key)

    def contextCellView[S <: Sys[S]](key: String)(implicit tx: S#Tx, context: Context[S]): CellView[S#Tx, Option[Folder]] =
      new AbstractCtxCellView[S, Folder](context.attr, key) {
        protected def tryParseValue(value: Any)(implicit tx: S#Tx): Option[Folder] = value match {
          case f: Folder  => Some(f)
          case _          => None
        }

        protected def tryParseObj(obj: stm.Obj[S])(implicit tx: S#Tx): Option[Folder] = obj match {
          case f: stm.Folder[S] => Some(wrap(f))
          case _                => None
        }
      }

    def cellValue[S <: Sys[S]](obj: stm.Obj[S], key: String)(implicit tx: S#Tx): Option[Folder] =
      obj.attr.$[stm.Folder](key).map(wrap(_))
  }

  private abstract class ExpandedImpl[S <: Sys[S], A](in: IExpr[S, Folder], init: A, tx0: S#Tx)
                                                     (implicit protected val targets: ITargets[S])
    extends IExpr[S, A] with IChangeGenerator[S, A] with Caching {

    private[this] val obs   = Ref[Disposable[S#Tx]](Disposable.empty)
    private[this] val ref   = Ref(init)

    protected def mapValue(f: stm.List[S, stm.Obj[S]])(implicit tx: S#Tx): A

    private def setObj(v: Folder)(implicit tx: S#Tx): Option[Change[A]] = {
      obs.swap(Disposable.empty).dispose()
      // XXX TODO --- should we also fire when size has been non-zero and v.peer is empty?
      v.peer.flatMap { f =>
        val newObs = f.changed.react { implicit tx => upd =>
          val now     = mapValue(upd.list)
          val before  = ref.swap(now)
          if (before != now) fire(Change(before, now))
        }
        obs() = newObs
        val now     = mapValue(f)
        val before  = ref.swap(now)
        if (before != now) Some(Change(before, now)) else None
      }
    }

    in.changed.--->(this)(tx0)
    setObj(in.value(tx0))(tx0)

    def value(implicit tx: S#Tx): A =
      IPush.tryPull(this).fold(ref())(_.now)

    def changed: IChangeEvent[S, A] = this

    def dispose()(implicit tx: S#Tx): Unit = {
      in.changed.-/->(this)
      obs.swap(Disposable.empty).dispose()
    }

    private[lucre] def pullChange(pull: IPull[S], isNow: Boolean)(implicit tx: S#Tx) = ???

//    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Change[A]] =
//      if (pull.isOrigin(this)) Some(pull.resolve)
//      else {
//        pull(in.changed).flatMap { ch =>
//          setObj(ch.now)
//        }
//      }
  }

  private final class SizeExpanded[S <: Sys[S]](in: IExpr[S, Folder], tx0: S#Tx)
                                               (implicit targets: ITargets[S])
    extends ExpandedImpl[S, Int](in, 0, tx0) {

    protected def mapValue(f: stm.List[S, stm.Obj[S]])(implicit tx: S#Tx): Int = f.size
  }

  final case class Size(in: Ex[Folder]) extends Ex[Int] {
    override def productPrefix: String = s"Folder$$Size" // serialization

    type Repr[S <: Sys[S]] = IExpr[S, Int]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new SizeExpanded(in.expand[S], tx)
    }
  }

  private final class IsEmptyExpanded[S <: Sys[S]](in: IExpr[S, Folder], tx0: S#Tx)
                                                  (implicit targets: ITargets[S])
    extends ExpandedImpl[S, Boolean](in, true, tx0) {

    protected def mapValue(f: stm.List[S, stm.Obj[S]])(implicit tx: S#Tx): Boolean = f.isEmpty
  }

  final case class IsEmpty(in: Ex[Folder]) extends Ex[Boolean] {
    override def productPrefix: String = s"Folder$$IsEmpty" // serialization

    type Repr[S <: Sys[S]] = IExpr[S, Boolean]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new IsEmptyExpanded(in.expand[S], tx)
    }
  }

  private final class NonEmptyExpanded[S <: Sys[S]](in: IExpr[S, Folder], tx0: S#Tx)
                                                   (implicit targets: ITargets[S])
    extends ExpandedImpl[S, Boolean](in, false, tx0) {

    protected def mapValue(f: stm.List[S, stm.Obj[S]])(implicit tx: S#Tx): Boolean = f.nonEmpty
  }

  final case class NonEmpty(in: Ex[Folder]) extends Ex[Boolean] {
    override def productPrefix: String = s"Folder$$NonEmpty" // serialization

    type Repr[S <: Sys[S]] = IExpr[S, Boolean]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new NonEmptyExpanded(in.expand[S], tx)
    }
  }

  private final class ChildrenExpanded[S <: Sys[S]](in: IExpr[S, Folder], tx0: S#Tx)
                                                   (implicit targets: ITargets[S])
    extends ExpandedImpl[S, Seq[Obj]](in, Nil, tx0) {

    protected def mapValue(f: stm.List[S, stm.Obj[S]])(implicit tx: S#Tx): Seq[Obj] = {
      val b = List.newBuilder[Obj]
      b.sizeHint(f.size)
      f.iterator.foreach { peer =>
        b += Obj.wrap(peer)
      }
      b.result()
    }
  }

  final case class Children(in: Ex[Folder]) extends Ex[Seq[Obj]] {
    override def productPrefix: String = s"Folder$$Children" // serialization

    type Repr[S <: Sys[S]] = IExpr[S, Seq[Obj]]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new ChildrenExpanded(in.expand[S], tx)
    }
  }

  private final class AppendExpanded[S <: Sys[S], A](in: IExpr[S, Folder], elem: IExpr[S, A])
                                                    (implicit source: Obj.Source[A])
    extends IActionImpl[S] {

    def executeAction()(implicit tx: S#Tx): Unit = {
      in.value.peer.foreach { f =>
        val v   = elem.value
        val obj = source.toObj(v)
        EditFolder.append(f, obj)
      }
    }
  }

  private final class PrependExpanded[S <: Sys[S], A](in: IExpr[S, Folder], elem: IExpr[S, A])
                                                     (implicit source: Obj.Source[A])
    extends IActionImpl[S] {

    def executeAction()(implicit tx: S#Tx): Unit = {
      in.value.peer.foreach { f =>
        val v   = elem.value
        val obj = source.toObj(v)
        EditFolder.prepend(f, obj)
      }
    }
  }

  final case class Append[A](in: Ex[Folder], elem: Ex[A])(implicit source: Obj.Source[A])
    extends Act with ProductWithAdjuncts {

    override def productPrefix: String = s"Folder$$Append" // serialization

    type Repr[S <: Sys[S]] = IAction[S]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] =
      new AppendExpanded(in.expand[S], elem.expand[S])

    def adjuncts: List[Adjunct] = source :: Nil
  }

  final case class Prepend[A](in: Ex[Folder], elem: Ex[A])(implicit source: Obj.Source[A])
    extends Act with ProductWithAdjuncts {

    override def productPrefix: String = s"Folder$$Prepend" // serialization

    type Repr[S <: Sys[S]] = IAction[S]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] =
      new PrependExpanded(in.expand[S], elem.expand[S])

    def adjuncts: List[Adjunct] = source :: Nil
  }

  implicit final class Ops(private val f: Ex[Folder]) extends AnyVal {
    def prepend[A](elem: Ex[A])(implicit source: Obj.Source[A]): Act = Prepend(f, elem)
    def append [A](elem: Ex[A])(implicit source: Obj.Source[A]): Act = Append (f, elem)

    def size    : Ex[Int    ]   = Size    (f)
    def isEmpty : Ex[Boolean]   = IsEmpty (f)
    def nonEmpty: Ex[Boolean]   = NonEmpty(f)
    
    def children: Ex[Seq[Obj]]  = Children(f)
  }
}
/** The representation of a folder within an expression program.
  * It allows to refer to existing folders through `"key".attr[Folder]`
  * or to create a prototype `Folder()` which has a `make` action.
  *
  * '''Note:''' passing a folder with `runWith` is not yet implemented.
  */
trait Folder extends Obj {
  type Peer[~ <: Sys[~]] = stm.Folder[~]
}