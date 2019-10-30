/*
 *  Obj.scala
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
import de.sciss.lucre.event.impl.IChangeGenerator
import de.sciss.lucre.event.{Caching, IChangeEvent, IPull, IPush, ITargets}
import de.sciss.lucre.expr.graph.impl.{AbstractCtxCellView, ExpandedAttrSetIn, ExpandedAttrUpdateIn, ObjCellViewVarImpl, ObjImplBase}
import de.sciss.lucre.expr.graph.{Attr => _Attr}
import de.sciss.lucre.expr.impl.{ExObjBridgeImpl, ExSeqObjBridgeImpl, ITriggerConsumer}
import de.sciss.lucre.expr.{BooleanObj, CellView, Context, DoubleObj, DoubleVector, IAction, IControl, IExpr, IntObj, IntVector, LongObj, SpanLikeObj, SpanObj, StringObj}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.stm.{Disposable, Sys}
import de.sciss.model.Change
import de.sciss.serial.{DataInput, Serializer}
import de.sciss.span.{Span => _Span, SpanLike => _SpanLike}

import scala.concurrent.stm.Ref
import scala.language.higherKinds

object Obj {
  private lazy val _init: Unit = {
    Adjunct.addFactory(Source.obj)
    Adjunct.addFactory(Bridge.obj)
  }

  def init(): Unit = _init

  implicit class ExOps(private val obj: Ex[Obj]) extends AnyVal {
    def attr[A: Bridge](key: String): Obj.Attr[A] = Obj.Attr(obj, key)

    // def attr[A: Bridge](key: String, default: Ex[A]): _Attr.WithDefault[A] = ...

    def copy: Copy = Obj.Copy(obj)
  }

  // used by Mellite (no transaction available)
  private[lucre] def wrapH[S <: Sys[S]](peer: stm.Source[S#Tx, stm.Obj[S]], system: S): Obj =
    new Impl[S](peer, system)

  private[lucre] def wrap[S <: Sys[S]](peer: stm.Obj[S])(implicit tx: S#Tx): Obj =
    new Impl[S](tx.newHandle(peer), tx.system)

  def empty: Ex[Obj] = Const(Empty)

  private[lucre] case object Empty extends Obj {
    override def productPrefix: String = s"Obj$$Empty$$"  // serialization
    override def toString     : String = "Obj<empty>"

    type Peer[~ <: Sys[~]] = stm.Obj[~]

    private[lucre] def peer[S <: Sys[S]](implicit tx: S#Tx): Option[Peer[S]] =
      None // throw new IllegalStateException("Object has not been created yet")
  }

  private final class Impl[In <: Sys[In]](in: stm.Source[In#Tx, stm.Obj[In]], system: In)
    extends ObjImplBase[In, stm.Obj](in, system) {

    override def toString: String = s"Obj($in)"
  }

  private abstract class AbstractMakeExpanded[S <: Sys[S]]
    extends IExpr[S, Obj]
      with IAction[S]
      with IChangeGenerator [S, Obj]
      with ITriggerConsumer [S, Obj]
      with Caching {

    // ---- abstract ----

    protected def make()(implicit tx: S#Tx): Obj

    // ---- impl ----

    private[this] val ref = Ref[Obj](Empty)

    final def value(implicit tx: S#Tx): Obj =
      IPush.tryPull(this).fold(ref())(_.now)

    final def executeAction()(implicit tx: S#Tx): Unit =
      trigReceived() // .foreach(fire) --- we don't need to fire, there is nobody listening;

    final protected def trigReceived()(implicit tx: S#Tx): Option[Change[Obj]] = {
      val now     = make()
      val before  = ref.swap(now) // needs caching
      if (before == now)
        None
      else
        Some(Change(before, now))
    }

    final def changed: IChangeEvent[S, Obj] = this
  }

  private final class MakeExpanded[S <: Sys[S], A](ex: IExpr[S, A])(implicit protected val targets: ITargets[S],
                                                                    cm: CanMake[A])
    extends AbstractMakeExpanded[S] {

    protected def make()(implicit tx: S#Tx): Obj = {
      val v     = ex.value
      val peer  = cm.toObj(v)
      wrap(peer)
    }
  }

  object Make {
    def apply[A](ex: Ex[A])(implicit cm: CanMake[A]): Make = Impl(ex)

    private final case class Impl[A](ex: Ex[A])(implicit cm: CanMake[A]) extends Make with Act with ProductWithAdjuncts {
      type Repr[S <: Sys[S]] = IExpr[S, Obj] with IAction[S]

      override def productPrefix: String = s"Obj$$Make" // serialization

      def make: Act = this

      def adjuncts: List[Adjunct] = cm :: Nil

      protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
        import ctx.targets
        new MakeExpanded(ex.expand[S])
      }
    }
  }
  trait Make extends Ex[Obj] {
    def make: Act
  }

  object Bridge {
    implicit val int      : Bridge[Int        ] = new ExObjBridgeImpl(IntObj       )
    implicit val long     : Bridge[Long       ] = new ExObjBridgeImpl(LongObj      )
    implicit val double   : Bridge[Double     ] = new ExObjBridgeImpl(DoubleObj    )
    implicit val boolean  : Bridge[Boolean    ] = new ExObjBridgeImpl(BooleanObj   )
    implicit val string   : Bridge[String     ] = new ExObjBridgeImpl(StringObj    )
    implicit val spanLike : Bridge[_SpanLike  ] = new ExObjBridgeImpl(SpanLikeObj  )
    implicit val span     : Bridge[_Span      ] = new ExObjBridgeImpl(SpanObj      )
    implicit val intVec   : Bridge[Seq[Int   ]] = new ExSeqObjBridgeImpl(IntVector    )
    implicit val doubleVec: Bridge[Seq[Double]] = new ExSeqObjBridgeImpl(DoubleVector )

    implicit object obj extends Bridge[Obj] with Adjunct.Factory {
      final val id = 1005

      type Repr[S <: Sys[S]] = stm.Obj[S]

      def readIdentifiedAdjunct(in: DataInput): Adjunct = this

      def cellView[S <: Sys[S]](obj: stm.Obj[S], key: String)(implicit tx: S#Tx): CellView.Var[S#Tx, Option[Obj]] =
        new ObjCellViewVarImpl[S, stm.Obj, Obj](tx.newHandle(obj), key) {
          protected def lower(peer: stm.Obj[S])(implicit tx: S#Tx): Obj =
            wrap(peer)
        }

      def contextCellView[S <: Sys[S]](key: String)(implicit tx: S#Tx, context: Context[S]): CellView[S#Tx, Option[Obj]] = {
        new AbstractCtxCellView[S, Obj](context.attr, key) {
          protected def tryParseValue(value: Any)(implicit tx: S#Tx): Option[Obj] = value match {
            case obj: Obj => Some(obj)
            case _        => None
          }

          protected def tryParseObj(peer: stm.Obj[S])(implicit tx: S#Tx): Option[Obj] =
            Some(wrap(peer))
        }
      }

      def cellValue[S <: Sys[S]](obj: stm.Obj[S], key: String)(implicit tx: S#Tx): Option[Obj] =
        obj.attr.get(key).map(wrap(_))
    }
  }
  trait Bridge[A] extends Adjunct {
    /** Creates a bidirectional view between `stm.Obj` and the expression side representation type `A`.
      * If possible, implementations should look at `UndoManager.find` when updating values.
      */
    def cellView[S <: Sys[S]](obj: stm.Obj[S], key: String)(implicit tx: S#Tx): CellView.Var[S#Tx, Option[A]]

    /** Creates a unidirectional view between a context's attribute or self object and the expression side
      * representation type `A`.
      */
    def contextCellView[S <: Sys[S]](key: String)(implicit tx: S#Tx, context: Context[S]): CellView[S#Tx, Option[A]]

    def cellValue[S <: Sys[S]](obj: stm.Obj[S], key: String)(implicit tx: S#Tx): Option[A]
  }

  object Source {
    implicit object obj extends Source[Obj] with Adjunct.Factory {
      final val id = 1006

      type Repr[S <: Sys[S]] = stm.Obj[S]

      def toObj[S <: Sys[S]](value: Obj)(implicit tx: S#Tx): stm.Obj[S] =
        value.peer.getOrElse(throw new IllegalStateException("Object has not yet been instantiated"))

      implicit def reprSerializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, stm.Obj[S]] =
        stm.Obj.serializer

      def readIdentifiedAdjunct(in: DataInput): Adjunct = this
    }

    implicit def canMake[A](implicit peer: CanMake[A]): Source[A] = peer
  }

  /** An `Obj.Source` either has an `stm.Obj` peer, or it can make one.
    * The latter is represented by sub-trait `CanMake`.
    */
  trait Source[-A] extends Adjunct {
    type Repr[S <: Sys[S]] <: stm.Obj[S]

    def toObj[S <: Sys[S]](value: A)(implicit tx: S#Tx): Repr[S]

    implicit def reprSerializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Repr[S]]
  }

  object CanMake {
    implicit val int      : CanMake[Int        ] = new ExObjBridgeImpl(IntObj       )
    implicit val long     : CanMake[Long       ] = new ExObjBridgeImpl(LongObj      )
    implicit val double   : CanMake[Double     ] = new ExObjBridgeImpl(DoubleObj    )
    implicit val boolean  : CanMake[Boolean    ] = new ExObjBridgeImpl(BooleanObj   )
    implicit val string   : CanMake[String     ] = new ExObjBridgeImpl(StringObj    )
    implicit val spanLike : CanMake[_SpanLike  ] = new ExObjBridgeImpl(SpanLikeObj  )
    implicit val span     : CanMake[_Span      ] = new ExObjBridgeImpl(SpanObj      )
    implicit val intVec   : CanMake[Seq[Int   ]] = new ExSeqObjBridgeImpl(IntVector    )
    implicit val doubleVec: CanMake[Seq[Double]] = new ExSeqObjBridgeImpl(DoubleVector )
  }
  trait CanMake[A] extends Source[A]

  private final class AttrExpanded[S <: Sys[S], A](obj: IExpr[S, Obj], key: String, tx0: S#Tx)
                                                  (implicit protected val targets: ITargets[S], bridge: Bridge[A])
    extends IExpr[S, Option[A]] with IChangeGenerator[S, Option[A]] {

    override def toString: String = s"graph.Obj.AttrExpanded($obj, $key)@${hashCode().toHexString}"

    private[this] val viewRef   = Ref(Option.empty[CellView.Var[S#Tx, Option[A]]])
    private[this] val valueRef  = Ref.make[Option[A]]
    private[this] val obsRef    = Ref.make[Disposable[S#Tx]]
    private[this] val objObs    = obj.changed.react { implicit tx => upd =>
      setObj(upd.now, init = false)
    } (tx0)

    private def setNewValue(now: Option[A])(implicit tx: S#Tx): Unit = {
      val before = valueRef.swap(now)
      if (before != now) {
        fire(Change(before, now))
      }
    }

    private def setObj(newObj: Obj, init: Boolean)(implicit tx: S#Tx): Unit = {
      // println(s"newObj = $newObj, bridge = $bridge, key = $key")
      val newView = newObj.peer.map(p => bridge.cellView(p, key))
      viewRef()   = newView
      val obsNew = newView.fold[Disposable[S#Tx]](Disposable.empty)(_.react { implicit tx => now =>
        setNewValue(now)
      })
      val now: Option[A] = newView.flatMap(_.apply())
      if (init) {
        obsRef  () = obsNew
        valueRef() = now
      } else {
        obsRef.swap(obsNew).dispose()
        setNewValue(now)
      }
    }

    // ---- init ----
    setObj(obj.value(tx0), init = true)(tx0)

    def value(implicit tx: S#Tx): Option[A] = viewRef().flatMap(_.apply())

    private[lucre] def pullChange(pull: IPull[S], isNow: Boolean)(implicit tx: S#Tx): Option[A] =
      pull.resolveChange(isNow = isNow)

//    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Change[Option[A]]] =
//      Some(pull.resolve)

    def changed: IChangeEvent[S, Option[A]] = this

    def dispose()(implicit tx: S#Tx): Unit = {
      objObs  .dispose()
      obsRef().dispose()
    }
  }

  object Attr {
    final case class Update[A](obj: Ex[Obj], key: String,value: Ex[A])(implicit bridge: Obj.Bridge[A])
      extends Control with ProductWithAdjuncts {

      override def productPrefix: String = s"Obj$$Attr$$Update"  // serialization

      type Repr[S <: Sys[S]] = IControl[S]

      protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
        val peer = new ExpandedAttrUpdateIn[S, A](obj.expand[S], key, value.expand[S], tx)
        IControl.wrap(peer)
      }

      override def adjuncts: scala.List[Adjunct] = bridge :: Nil
    }
    final case class Set[A](obj: Ex[Obj], key: String, value: Ex[A])(implicit bridge: Obj.Bridge[A])
      extends Act with ProductWithAdjuncts {

      override def productPrefix: String = s"Obj$$Attr$$Set"  // serialization

      type Repr[S <: Sys[S]] = IAction[S]

      protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] =
        new ExpandedAttrSetIn[S, A](obj.expand[S], key, value.expand[S], tx)

      override def adjuncts: scala.List[Adjunct] = bridge :: Nil
    }
  }

  // XXX TODO --- this should be merged with graph.Attr ?
  final case class Attr[A](obj: Ex[Obj], key: String)(implicit val bridge: Bridge[A])
    extends Ex[Option[A]] with _Attr.Like[A] {

    type Repr[S <: Sys[S]] = IExpr[S, Option[A]]

    override def productPrefix: String = s"Obj$$Attr" // serialization

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new AttrExpanded(obj.expand[S], key, tx)
    }

    def update(in: Ex[A]): Control  = Obj.Attr.Update (obj, key, in)
    def set   (in: Ex[A]): Act      = Obj.Attr.Set    (obj, key, in)

    def adjuncts: List[Adjunct] = bridge :: Nil
  }

  private final class CopyExpanded[S <: Sys[S]](ex: IExpr[S, Obj])(implicit protected val targets: ITargets[S])
    extends AbstractMakeExpanded[S] {

    protected def make()(implicit tx: S#Tx): Obj = {
      val v = ex.value
      v.peer match {
        case Some(orig) =>
          val cpy: stm.Obj[S] = stm.Obj.copy(orig)
          wrap(cpy)

        case None => Empty
      }
    }
  }

  object Copy {
    def apply(obj: Ex[Obj]): Copy = Impl(obj)

    private final case class Impl(obj: Ex[Obj])
      extends Copy with Act {

      type Repr[S <: Sys[S]] = IExpr[S, Obj] with IAction[S]

      def make: Act = this

      override def productPrefix: String = s"Obj$$Copy" // serialization

      protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
        import ctx.targets
        new CopyExpanded[S](obj.expand[S])
      }
    }
  }
  type Copy = Make
  // trait Copy extends Make
}
trait Obj {
  type Peer[~ <: Sys[~]] <: stm.Obj[~]

  private[lucre] def peer[S <: Sys[S]](implicit tx: S#Tx): Option[Peer[S]]
}
