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

import de.sciss.lucre.aux.{Aux, ProductWithAux}
import de.sciss.lucre.event.impl.IGenerator
import de.sciss.lucre.event.{Caching, IEvent, IPull, ITargets}
import de.sciss.lucre.expr.graph.impl.ObjImpl
import de.sciss.lucre.expr.graph.{Attr => _Attr}
import de.sciss.lucre.expr.impl.{ExObjBridgeImpl, ITriggerConsumer}
import de.sciss.lucre.expr.{BooleanObj, CellView, Context, DoubleObj, DoubleVector, IAction, IExpr, IntObj, IntVector, LongObj, SpanLikeObj, SpanObj, StringObj}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.stm.{Disposable, Sys}
import de.sciss.model.Change
import de.sciss.serial.Serializer
import de.sciss.span.{Span, SpanLike}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.Ref
import scala.language.higherKinds

object Obj {
  implicit class ExOps(private val obj: Ex[Obj]) extends AnyVal {
    def attr[A: Bridge](key: String): Obj.Attr[A] = Obj.Attr(obj, key)

    def attr[A: Bridge](key: String, default: Ex[A]): _Attr.WithDefault[A] = ???
  }

  private object Empty extends Obj {
    private[graph] def peer[S <: Sys[S]](implicit tx: S#Tx): stm.Obj[S] =
      throw new IllegalStateException("Object has not been created yet")
  }

  private final class MakeExpanded[S <: Sys[S], A](ex: IExpr[S, A])(implicit protected val targets: ITargets[S],
                                                                    bridge: Bridge[A])
    extends IExpr[S, Obj]
      with IAction[S]
      with IGenerator       [S, Change[Obj]]
      with ITriggerConsumer [S, Change[Obj]]
      with Caching {

    private[this] val ref = Ref[Obj](Empty)

    def value(implicit tx: S#Tx): Obj = ref()

    def executeAction()(implicit tx: S#Tx): Unit =
      trigReceived().foreach(fire)

    private def make()(implicit tx: S#Tx): Obj = {
      val v     = ex.value
      val peer  = bridge.mkObj(v)
      import bridge.reprSerializer
      new ObjImpl(tx.newHandle(peer), tx.system)
    }

    protected def trigReceived()(implicit tx: S#Tx): Option[Change[Obj]] = {
      val now     = make()
      val before  = ref.swap(now)
      Some(Change(before, now))
    }

    def changed: IEvent[S, Change[Obj]] = this
  }

  final case class Make[A](ex: Ex[A])(implicit bridge: Bridge[A]) extends Ex[Obj] with Act with ProductWithAux {
    type Repr[S <: Sys[S]] = IExpr[S, Obj] with IAction[S]

    override def productPrefix: String = s"Obj$$Make" // serialization

    def aux: List[Aux] = bridge :: Nil

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new MakeExpanded(ex.expand[S])
    }
  }

  object Bridge {
    implicit val int      : Bridge[Int        ] = new ExObjBridgeImpl(IntObj       )
    implicit val long     : Bridge[Long       ] = new ExObjBridgeImpl(LongObj      )
    implicit val double   : Bridge[Double     ] = new ExObjBridgeImpl(DoubleObj    )
    implicit val boolean  : Bridge[Boolean    ] = new ExObjBridgeImpl(BooleanObj   )
    implicit val string   : Bridge[String     ] = new ExObjBridgeImpl(StringObj    )
    implicit val spanLike : Bridge[SpanLike   ] = new ExObjBridgeImpl(SpanLikeObj  )
    implicit val span     : Bridge[Span       ] = new ExObjBridgeImpl(SpanObj      )
    implicit val intVec   : Bridge[Vec[Int   ]] = new ExObjBridgeImpl(IntVector    )
    implicit val doubleVec: Bridge[Vec[Double]] = new ExObjBridgeImpl(DoubleVector )

  }
  trait Bridge[A] extends Aux {
    type Repr[S <: Sys[S]] <: stm.Obj[S]

    def mkObj[S <: Sys[S]](value: A)(implicit tx: S#Tx): Repr[S]

    implicit def reprSerializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Repr[S]]

    def cellView[S <: Sys[S]](obj: stm.Obj[S], key: String)(implicit tx: S#Tx): CellView.Var[S, Option[A]]
  }

  private final class AttrExpanded[S <: Sys[S], A](obj: IExpr[S, Obj], key: String, tx0: S#Tx)
                                                  (implicit protected val targets: ITargets[S], bridge: Bridge[A])
    extends IExpr[S, Option[A]] with IGenerator[S, Change[Option[A]]] {

    private[this] val viewRef   = Ref.make[CellView.Var[S, Option[A]]]
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
      val newView = bridge.cellView(newObj.peer, key)
      viewRef()   = newView
      val obsNew = newView.react { implicit tx => now =>
        setNewValue(now)
      }
      val now = newView()
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

    def value(implicit tx: S#Tx): Option[A] = viewRef.get.apply()

    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Change[Option[A]]] =
      Some(pull.resolve)

    def changed: IEvent[S, Change[Option[A]]] = this

    def dispose()(implicit tx: S#Tx): Unit = {
      objObs  .dispose()
      obsRef().dispose()
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

    def update(in: Ex[A]): Control  = ???
    def set   (in: Ex[A]): Act      = ???

    def aux: List[Aux] = bridge :: Nil
  }
}
trait Obj {
  private[graph] def peer[S <: Sys[S]](implicit tx: S#Tx): stm.Obj[S]
}
