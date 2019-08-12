/*
 *  Obj.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2019 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr.graph

import de.sciss.lucre.aux.Aux.FromAny
import de.sciss.lucre.aux.{Aux, ProductWithAux}
import de.sciss.lucre.event.impl.IGenerator
import de.sciss.lucre.event.{Caching, IEvent, IPull, IPush, ITargets}
import de.sciss.lucre.expr.graph.impl.{ExpandedAttrSetIn, ExpandedAttrUpdateIn, ObjCellViewImpl, ObjImplBase}
import de.sciss.lucre.expr.graph.{Attr => _Attr}
import de.sciss.lucre.expr.impl.{ExObjBridgeImpl, ExObjCanMakeImpl, ITriggerConsumer}
import de.sciss.lucre.expr.{BooleanObj, CellView, Context, DoubleObj, DoubleVector, IAction, IControl, IExpr, IntObj, IntVector, LongObj, SpanLikeObj, SpanObj, StringObj}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.stm.{Disposable, Sys}
import de.sciss.model.Change
import de.sciss.serial.{DataInput, Serializer}
import de.sciss.span.{Span => _Span, SpanLike => _SpanLike}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.Ref
import scala.language.higherKinds

object Obj {
  private lazy val _init: Unit =
    Aux.addFactory(Source.obj)
    Aux.addFactory(Bridge.obj)
    Aux.addFactory(Bridge.int)
    Aux.addFactory(Bridge.intVec)
    Aux.addFactory(Bridge.double)
    Aux.addFactory(Bridge.doubleVec)
    Aux.addFactory(Bridge.boolean)
    Aux.addFactory(Bridge.long)
    Aux.addFactory(Bridge.spanLike)
    Aux.addFactory(Bridge.span)
    Aux.addFactory(Bridge.string)

  def init(): Unit = _init

  implicit class ExOps(private val obj: Ex[Obj]) extends AnyVal {
    def attr[A: Bridge](key: String): Obj.Attr[A] = Obj.Attr(obj, key)

    // def attr[A: Bridge](key: String, default: Ex[A]): _Attr.WithDefault[A] = ...
  }

  private[lucre] def wrap[S <: Sys[S]](peer: stm.Source[S#Tx, stm.Obj[S]], system: S): Obj =
    new Impl[S](peer, system)

  private[lucre] object Empty extends Obj {
    type Peer[~ <: Sys[~]] = stm.Obj[~]

    private[lucre] def peer[S <: Sys[S]](implicit tx: S#Tx): Option[Peer[S]] =
      None // throw new IllegalStateException("Object has not been created yet")
  }

  final class Impl[In <: Sys[In]](in: stm.Source[In#Tx, stm.Obj[In]], system: In)
    extends ObjImplBase[In, stm.Obj](in, system)

  private final class MakeExpanded[S <: Sys[S], A](ex: IExpr[S, A])(implicit protected val targets: ITargets[S],
                                                                    cm: CanMake[A])
    extends IExpr[S, Obj]
      with IAction[S]
      with IGenerator       [S, Change[Obj]]
      with ITriggerConsumer [S, Change[Obj]]
      with Caching {

    private[this] val ref = Ref[Obj](Empty)

    def value(implicit tx: S#Tx): Obj =
      IPush.tryPull(this).fold(ref())(_.now)

    def executeAction()(implicit tx: S#Tx): Unit =
      trigReceived() // .foreach(fire) --- we don't need to fire, there is nobody listening;

    private def make()(implicit tx: S#Tx): Obj = {
      val v     = ex.value
      val peer  = cm.toObj(v)
      import cm.reprSerializer
      wrap(tx.newHandle(peer), tx.system)
    }

    protected def trigReceived()(implicit tx: S#Tx): Option[Change[Obj]] = {
      val now     = make()
      val before  = ref.swap(now) // needs caching
      Some(Change(before, now))
    }

    def changed: IEvent[S, Change[Obj]] = this
  }

  object Make {
    def apply[A](ex: Ex[A])(implicit cm: CanMake[A]): Make[A] = Impl(ex)

    private final case class Impl[A](ex: Ex[A])(implicit cm: CanMake[A]) extends Make[A] with Act with ProductWithAux {
      type Repr[S <: Sys[S]] = IExpr[S, Obj] with IAction[S]

      override def productPrefix: String = s"Obj$$Make" // serialization

      def make: Act = this

      def aux: List[Aux] = cm :: Nil

      protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
        import ctx.targets
        new MakeExpanded(ex.expand[S])
      }
    }
  }
  trait Make[A] extends Ex[Obj] {
    def make: Act
  }

  object Bridge {
    implicit object obj extends Bridge[Obj] with Aux.Factory {
      final val id = 1005

      type Repr[S <: Sys[S]] = stm.Obj[S]

      //      def mkObj[S <: Sys[S]](value: Obj)(implicit tx: S#Tx): stm.Obj[S] = ...

      //      implicit def reprSerializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, stm.Obj[S]] =
      //        stm.Obj.serializer

      def readIdentifiedAux(in: DataInput): Aux = this

      def cellView[S <: Sys[S]](obj: stm.Obj[S], key: String)(implicit tx: S#Tx): CellView.Var[S, Option[Obj]] =
        new ObjCellViewImpl[S, stm.Obj, Obj](tx.newHandle(obj), key) {
          protected def lower(peer: stm.Obj[S])(implicit tx: S#Tx): Obj =
            wrap[S](tx.newHandle(peer), tx.system)

          implicit def serializer: Serializer[S#Tx, S#Acc, Option[stm.Obj[S]]] =
            Serializer.option
        }

      def cellView[S <: Sys[S]](key: String)(implicit tx: S#Tx, context: Context[S]): CellView[S#Tx, Option[Obj]] = ???
    }

    implicit object int extends Bridge[Int] with Aux.Factory { factory =>
      final val id = 1010

      def cellView[S <: Sys[S]](obj: stm.Obj[S], key: String)(implicit tx: S#Tx): CellView.Var[S, Option[Int]] =
        Impl.cellView(obj, key)

      def cellView[S <: Sys[S]](key: String)(implicit tx: S#Tx, context: Context[S]): CellView[S#Tx, Option[Int]] =
        Impl.cellView(key)

      def readIdentifiedAux(in: DataInput): Aux = this

      private object Impl extends ExObjBridgeImpl(IntObj) {
        def id: Int = factory.id
      }
    }

    implicit object intVec extends Bridge[Vec[Int]] with Aux.Factory { factory =>
      final val id = 1011

      def cellView[S <: Sys[S]](obj: stm.Obj[S], key: String)(implicit tx: S#Tx): CellView.Var[S, Option[Vec[Int]]] =
        Impl.cellView(obj, key)

      def cellView[S <: Sys[S]](key: String)(implicit tx: S#Tx, context: Context[S]): CellView[S#Tx, Option[Vec[Int]]] =
        Impl.cellView(key)

      def readIdentifiedAux(in: DataInput): Aux = this

      private object Impl extends ExObjBridgeImpl(IntVector)(FromAny.empty) { // XXX TODO --- FromAny
        def id: Int = factory.id
      }
    }

    implicit object double extends Bridge[Double] with Aux.Factory { factory =>
      final val id = 1012

      def cellView[S <: Sys[S]](obj: stm.Obj[S], key: String)(implicit tx: S#Tx): CellView.Var[S, Option[Double]] =
        Impl.cellView(obj, key)

      def cellView[S <: Sys[S]](key: String)(implicit tx: S#Tx, context: Context[S]): CellView[S#Tx, Option[Double]] =
        Impl.cellView(key)

      def readIdentifiedAux(in: DataInput): Aux = this

      private object Impl extends ExObjBridgeImpl(DoubleObj) {
        def id: Int = factory.id
      }
    }

    implicit object doubleVec extends Bridge[Vec[Double]] with Aux.Factory { factory =>
      final val id = 1013

      def cellView[S <: Sys[S]](obj: stm.Obj[S], key: String)(implicit tx: S#Tx): CellView.Var[S, Option[Vec[Double]]] =
        Impl.cellView(obj, key)

      def cellView[S <: Sys[S]](key: String)(implicit tx: S#Tx, context: Context[S]): CellView[S#Tx, Option[Vec[Double]]] =
        Impl.cellView(key)

      def readIdentifiedAux(in: DataInput): Aux = this

      private object Impl extends ExObjBridgeImpl(DoubleVector)(FromAny.empty) { // XXX TODO --- FromAny
        def id: Int = factory.id
      }
    }

    implicit object boolean extends Bridge[Boolean] with Aux.Factory { factory =>
      final val id = 1014

      def cellView[S <: Sys[S]](obj: stm.Obj[S], key: String)(implicit tx: S#Tx): CellView.Var[S, Option[Boolean]] =
        Impl.cellView(obj, key)

      def cellView[S <: Sys[S]](key: String)(implicit tx: S#Tx, context: Context[S]): CellView[S#Tx, Option[Boolean]] =
        Impl.cellView(key)

      def readIdentifiedAux(in: DataInput): Aux = this

      private object Impl extends ExObjBridgeImpl(BooleanObj) {
        def id: Int = factory.id
      }
    }

    implicit object long extends Bridge[Long] with Aux.Factory { factory =>
      final val id = 1016

      def cellView[S <: Sys[S]](obj: stm.Obj[S], key: String)(implicit tx: S#Tx): CellView.Var[S, Option[Long]] =
        Impl.cellView(obj, key)

      def cellView[S <: Sys[S]](key: String)(implicit tx: S#Tx, context: Context[S]): CellView[S#Tx, Option[Long]] =
        Impl.cellView(key)

      def readIdentifiedAux(in: DataInput): Aux = this

      private object Impl extends ExObjBridgeImpl(LongObj) {
        def id: Int = factory.id
      }
    }

    implicit object spanLike extends Bridge[_SpanLike] with Aux.Factory { factory =>
      final val id = 1018

      def cellView[S <: Sys[S]](obj: stm.Obj[S], key: String)(implicit tx: S#Tx): CellView.Var[S, Option[_SpanLike]] =
        Impl.cellView(obj, key)

      def cellView[S <: Sys[S]](key: String)(implicit tx: S#Tx, context: Context[S]): CellView[S#Tx, Option[_SpanLike]] =
        Impl.cellView(key)

      def readIdentifiedAux(in: DataInput): Aux = this

      private object Impl extends ExObjBridgeImpl(SpanLikeObj)(FromAny.empty) { // XXX TODO --- FromAny
        def id: Int = factory.id
      }
    }

    implicit object span extends Bridge[_Span] with Aux.Factory { factory =>
      final val id = 1019

      def cellView[S <: Sys[S]](obj: stm.Obj[S], key: String)(implicit tx: S#Tx): CellView.Var[S, Option[_Span]] =
        Impl.cellView(obj, key)

      def cellView[S <: Sys[S]](key: String)(implicit tx: S#Tx, context: Context[S]): CellView[S#Tx, Option[_Span]] =
        Impl.cellView(key)

      def readIdentifiedAux(in: DataInput): Aux = this

      private object Impl extends ExObjBridgeImpl(SpanObj)(FromAny.empty) { // XXX TODO --- FromAny
        def id: Int = factory.id
      }
    }

    implicit object string extends Bridge[String] with Aux.Factory { factory =>
      final val id = 1020

      def cellView[S <: Sys[S]](obj: stm.Obj[S], key: String)(implicit tx: S#Tx): CellView.Var[S, Option[String]] =
        Impl.cellView(obj, key)

      def cellView[S <: Sys[S]](key: String)(implicit tx: S#Tx, context: Context[S]): CellView[S#Tx, Option[String]] =
        Impl.cellView(key)

      def readIdentifiedAux(in: DataInput): Aux = this

      private object Impl extends ExObjBridgeImpl(StringObj) {
        def id: Int = factory.id
      }
    }
  }
  trait Bridge[A] extends Aux {
//    type Repr[S <: Sys[S]] <: stm.Obj[S]

    /** Creates a bidirectional view between `stm.Obj` and the expression side representation type `A`.
      * If possible, implementations should look at `UndoManager.find` when updating values.
      */
    def cellView[S <: Sys[S]](obj: stm.Obj[S], key: String)(implicit tx: S#Tx): CellView.Var[S, Option[A]]

    /** Creates a unidirectional view between a context's attribute or self object and the expression side
      * representation type `A`.
      */
    def cellView[S <: Sys[S]](key: String)(implicit tx: S#Tx, context: Context[S]): CellView[S#Tx, Option[A]]
  }

  object Source {
    implicit object obj extends Source[Obj] with Aux.Factory {
      final val id = 1006

      type Repr[S <: Sys[S]] = stm.Obj[S]

      def toObj[S <: Sys[S]](value: Obj)(implicit tx: S#Tx): stm.Obj[S] =
        value.peer.getOrElse(throw new IllegalStateException("Object has not yet been instantiated"))

      implicit def reprSerializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, stm.Obj[S]] =
        stm.Obj.serializer

      def readIdentifiedAux(in: DataInput): Aux = this
    }
  }

  /** An `Obj.Source` either has an `stm.Obj` peer, or it can make one.
    * The latter is represented by sub-trait `CanMake`.
    */
  trait Source[-A] extends Aux {
    type Repr[S <: Sys[S]] <: stm.Obj[S]

    def toObj[S <: Sys[S]](value: A)(implicit tx: S#Tx): Repr[S]

    implicit def reprSerializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Repr[S]]
  }

  object CanMake {
    implicit val int      : CanMake[Int        ] = new ExObjCanMakeImpl(IntObj       )
    implicit val long     : CanMake[Long       ] = new ExObjCanMakeImpl(LongObj      )
    implicit val double   : CanMake[Double     ] = new ExObjCanMakeImpl(DoubleObj    )
    implicit val boolean  : CanMake[Boolean    ] = new ExObjCanMakeImpl(BooleanObj   )
    implicit val string   : CanMake[String     ] = new ExObjCanMakeImpl(StringObj    )
    implicit val spanLike : CanMake[_SpanLike  ] = new ExObjCanMakeImpl(SpanLikeObj  )
    implicit val span     : CanMake[_Span      ] = new ExObjCanMakeImpl(SpanObj      )
    implicit val intVec   : CanMake[Vec[Int   ]] = new ExObjCanMakeImpl(IntVector    )
    implicit val doubleVec: CanMake[Vec[Double]] = new ExObjCanMakeImpl(DoubleVector )
  }
  trait CanMake[A] extends Source[A]

  private final class AttrExpanded[S <: Sys[S], A](obj: IExpr[S, Obj], key: String, tx0: S#Tx)
                                                  (implicit protected val targets: ITargets[S], bridge: Bridge[A])
    extends IExpr[S, Option[A]] with IGenerator[S, Change[Option[A]]] {

    override def toString: String = s"graph.Obj.AttrExpanded($obj, $key)@${hashCode().toHexString}"

    private[this] val viewRef   = Ref(Option.empty[CellView.Var[S, Option[A]]])
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

    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Change[Option[A]]] =
      Some(pull.resolve)

    def changed: IEvent[S, Change[Option[A]]] = this

    def dispose()(implicit tx: S#Tx): Unit = {
      objObs  .dispose()
      obsRef().dispose()
    }
  }

  object Attr {
    final case class Update[A](obj: Ex[Obj], key: String,value: Ex[A])(implicit bridge: Obj.Bridge[A])
      extends Control with ProductWithAux {

      override def productPrefix: String = s"Obj$$Attr$$Update"  // serialization

      type Repr[S <: Sys[S]] = IControl[S]

      protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
        val peer = new ExpandedAttrUpdateIn[S, A](obj.expand[S], key, value.expand[S], tx)
        IControl.wrap(peer)
      }

      override def aux: scala.List[Aux] = bridge :: Nil
    }
    final case class Set[A](obj: Ex[Obj], key: String, value: Ex[A])(implicit bridge: Obj.Bridge[A])
      extends Act with ProductWithAux {

      override def productPrefix: String = s"Obj$$Attr$$Set"  // serialization

      type Repr[S <: Sys[S]] = IAction[S]

      protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] =
        new ExpandedAttrSetIn[S, A](obj.expand[S], key, value.expand[S], tx)

      override def aux: scala.List[Aux] = bridge :: Nil
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

    def aux: List[Aux] = bridge :: Nil
  }
}
trait Obj {
  type Peer[~ <: Sys[~]] <: stm.Obj[~]

  private[lucre] def peer[S <: Sys[S]](implicit tx: S#Tx): Option[Peer[S]]
}
