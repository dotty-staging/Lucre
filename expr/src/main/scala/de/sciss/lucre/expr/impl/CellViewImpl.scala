/*
 *  CellViewImpl.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2020 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr.impl

import de.sciss.lucre.edit.{EditAttrMap, EditExprVar}
import de.sciss.lucre.expr.{CellView, Type, Expr => _Expr, ExprLike => _ExprLike}
import de.sciss.lucre.stm.Obj.AttrMap
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.stm.{Disposable, Form, MapLike, Obj, Sys}
import de.sciss.lucre.{stm, event => evt}
import de.sciss.model.Change
import de.sciss.serial.Serializer

import scala.concurrent.stm.Ref
import scala.language.higherKinds

object CellViewImpl {
  def const[S <: Sys[S], A](value: A): CellView[S#Tx, A] = new Const(value)

  def expr[S <: Sys[S], A, _Ex[~ <: Sys[~]] <: _Expr[~, A]](x: _Ex[S])(implicit tx: S#Tx,
                                                                       tpe: Type.Expr[A, _Ex]): CellView[S#Tx, A] = {
    import tpe.{serializer, varSerializer}
    x match {
      case tpe.Var(vr) =>
        new ExprVar[S, A, _Ex](tx.newHandle(vr))

      case _ =>
        new Expr[S, A, _Ex](tx.newHandle(x))
    }
  }

  def exprMap[S <: Sys[S], K, A, _Ex[~ <: Sys[~]] <: _Expr[~, A]](map: evt.Map[S, K, _Ex], key: K)
                                                                     (implicit tx: S#Tx, tpe: Type.Expr[A, _Ex], keyType: evt.Map.Key[K])
  : CellView[S#Tx, Option[A]] = {
    // import tpe.serializer
    map.modifiableOption.fold[CellView[S#Tx, Option[A]]] {
      new ExprMap[S, K, A, _Ex  /* , Change[A] */](tx.newHandle(map), key /* , ch =>
        if (ch.isSignificant) Some(ch.now) else None */)
    } { mv =>
      new ExprModMap[S, K, A, _Ex](tx.newHandle(mv), key)
    }
  }

  def attr[S <: Sys[S], A, E[~ <: Sys[~]] <: _Expr[~, A]](map: Obj.AttrMap[S], key: String)
                                                        (implicit tx: S#Tx, tpe: Type.Expr[A, E]): CellView.Var[S#Tx, Option[A]] =
    new PlainAttrImpl[S, A, E](tx.newHandle(map), key)

  /** Additionally uses undo manager when present. */
  def attrUndoOpt[S <: Sys[S], A, E[~ <: Sys[~]] <: _Expr[~, A]](map: Obj.AttrMap[S], key: String)
                                                               (implicit tx: S#Tx, tpe: Type.Expr[A, E]): CellView.Var[S#Tx, Option[A]] =
    new UndoAttrImpl[S, A, E](tx.newHandle(map), key)

  def exprLike[S <: Sys[S], A, _Ex[~ <: Sys[~]] <: _Expr[~, A]](x: _Ex[S])
                                                              (implicit tx: S#Tx,
                                                               serializer: Serializer[S#Tx, S#Acc, _Ex[S]]): CellView[S#Tx, A] =
    new Expr[S, A, _Ex](tx.newHandle(x))

  // ---- impl ----

  private[lucre] trait ExprMapLike[S <: Sys[S], K, A, _Ex[~ <: Sys[~]] <: _Expr[~, A] /* , U */]
    extends CellView[S#Tx, Option[A]] {

    protected def h: stm.Source[S#Tx, evt.Map[S, K, _Ex]]
    protected val key: K
    // protected def mapUpdate(u: U): Option[A]

    type Repr = Option[_Ex[S]]

    def react(fun: S#Tx => Option[A] => Unit)(implicit tx: S#Tx): Disposable[S#Tx] =
      new ExprMapLikeObs(h(), key, fun, tx)

    protected def repr(implicit tx: S#Tx): Repr

    def apply()(implicit tx: S#Tx): Option[A] = repr.map(_.value)
  }

  private final class ExprMapLikeObs[S <: Sys[S], K, A, _Ex[~ <: Sys[~]] <: _Expr[~, A], U](
      map: evt.Map[S, K, _Ex], key: K, fun: S#Tx => Option[A] => Unit, tx0: S#Tx)
    extends Disposable[S#Tx] {

    private val valObs = Ref(null: Disposable[S#Tx])

    private val mapObs = map.changed.react { implicit tx => u =>
      u.changes.foreach {
        case evt.Map.Added  (`key`, expr) =>
          valueAdded(expr)
          // XXX TODO -- if we moved this into `valueAdded`, the contract
          // could be that initially the view is updated
          val now0 = expr.value
          fun(tx)(Some(now0))
        case evt.Map.Removed(`key`, _ ) =>
          if (valueRemoved()) fun(tx)(None)
        case _ =>
      }
    } (tx0)

    map.get(key)(tx0).foreach(valueAdded(_)(tx0))

    private def valueAdded(expr: _Ex[S])(implicit tx: S#Tx): Unit = {
      val res = expr.changed.react { implicit tx => {
        case Change(_, now) =>
          fun(tx)(Some(now))
        //            val opt = mapUpdate(ch)
        //            if (opt.isDefined) fun(tx)(opt)
        case _ =>  // XXX TODO -- should we ask for expr.value ?
      }}
      val v = valObs.swap(res)
      if (v != null) v.dispose()
    }

    private def valueRemoved()(implicit tx: S#Tx): Boolean = {
      val v   = valObs.swap(null)
      val res = v != null
      if (res) v.dispose()
      res
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      valueRemoved()
      mapObs.dispose()
    }
  }

  private final class ExprMap[S <: Sys[S], K, A, _Ex[~ <: Sys[~]] <: _Expr[~, A] /* , U */](
      protected val h: stm.Source[S#Tx, evt.Map[S, K, _Ex]],
      protected val key: K /* , val updFun: U => Option[A] */)
    extends ExprMapLike[S, K, A, _Ex /* , U */] {

    override def repr(implicit tx: S#Tx): Repr = h().get(key)

    // protected def mapUpdate(u: U): Option[A] = updFun(u)
  }

  private final class ExprModMap[S <: Sys[S], K, A, _Ex[~ <: Sys[~]] <: _Expr[~, A]](
      protected val h: stm.Source[S#Tx, evt.Map.Modifiable[S, K, _Ex]],
      protected val key: K)
     (implicit tpe: Type.Expr[A, _Ex])
    extends ExprMapLike[S, K, A, _Ex /* , Change[A] */] with CellView.VarR[S, Option[A]] {

    def serializer: Serializer[S#Tx, S#Acc, Repr] = {
      implicit val exSer: Serializer[S#Tx, S#Acc, _Ex[S]] = tpe.serializer[S]
      Serializer.option[S#Tx, S#Acc, _Ex[S]]
    }

    // protected def mapUpdate(ch: Change[A]): Option[A] = if (ch.isSignificant) Some(ch.now) else None

    override def repr(implicit tx: S#Tx): Repr = {
      val opt = h().get(key)
      // ! important to unwrap, otherwise we get infinite recursion with `repr = repr` !
      opt.map {
        case tpe.Var(vr) =>
          vr()
        case other => other
      }
    }

    def repr_=(value: Repr)(implicit tx: S#Tx): Unit = value.fold[Unit] {
      h().remove(key)
    } { ex =>
      val map = h()
      map.get(key) match {
        case Some(tpe.Var(vr)) => vr() = ex
        case _ =>
          val exV = tpe.Var.unapply(ex).getOrElse(tpe.newVar(ex))
          map.put(key, exV)
      }
    }

    def lift(value: Option[A])(implicit tx: S#Tx): Repr = value.map(tpe.newConst[S](_))

    def update(v: Option[A])(implicit tx: S#Tx): Unit = repr_=(lift(v))
  }

  private[lucre] trait ExprLike[S <: Sys[S], A, _Ex[~ <: Sys[~]] <: _Expr[~, A]]
    extends CellView[S#Tx, A] {

    type Repr = _Ex[S]

    protected def h: stm.Source[S#Tx, Repr]

    def react(fun: S#Tx => A => Unit)(implicit tx: S#Tx): Disposable[S#Tx] =
      h().changed.react { implicit tx => ch => fun(tx)(ch.now) }

    def apply()(implicit tx: S#Tx): A = h().value
  }

  private final class Expr[S <: Sys[S], A, _Ex[~ <: Sys[~]] <: _Expr[~, A]](
                                                                                       protected val h: stm.Source[S#Tx, _Ex[S]])
    extends ExprLike[S, A, _Ex]

  private final class ExprVar[S <: Sys[S], A, _Ex[~ <: Sys[~]] <: _Expr[~, A]](
                                                                                          protected val h: stm.Source[S#Tx, _Ex[S] with stm.Var[S#Tx, _Ex[S]]])
                                                                                        (implicit tpe: Type.Expr[A, _Ex])
    extends ExprLike[S, A, _Ex] with CellView.VarR[S, A] {

    // ! important to unwrap, otherwise we get infinite recursion with `repr = repr` !
    override def repr(implicit tx: S#Tx): Repr = h().apply()

    def repr_=(value: Repr)(implicit tx: S#Tx): Unit = h().update(value)

    def lift(value: A)(implicit tx: S#Tx): Repr = tpe.newConst(value)

    def update(v: A)(implicit tx: S#Tx): Unit = repr_=(lift(v))

    def serializer: Serializer[S#Tx, S#Acc, Repr] = tpe.serializer
  }

  private[lucre] final class MapImpl[Tx, A, B](in: CellView[Tx, A], f: A => B)
    extends CellView[Tx, B] {

    def react(fun: Tx => B => Unit)(implicit tx: Tx): Disposable[Tx] =
      in.react { implicit tx => a => fun(tx)(f(a)) }

    def apply()(implicit tx: Tx): B = f(in())
  }

  private[lucre] final class OptionOrElseImpl[Tx, A](first : CellView[Tx, Option[A]],
                                                     second: CellView[Tx, Option[A]])
    extends CellView[Tx, Option[A]] {

    def apply()(implicit tx: Tx): Option[A] =
      first() orElse second()

    def react(fun: Tx => Option[A] => Unit)(implicit tx: Tx): Disposable[Tx] = {
      val r1 = first  .react { implicit tx => opt => fun(tx)(opt      orElse second() ) }
      val r2 = second .react { implicit tx => opt => fun(tx)(first()  orElse opt      ) }
      Disposable.seq(r1, r2)
    }
  }

  private[lucre] final class FlatMapImpl[Tx, A, B](in: CellView[Tx, Option[A]], f: Tx => A => Option[B])
    extends CellView[Tx, Option[B]] {

    def apply()(implicit tx: Tx): Option[B] =
      in().flatMap(f(tx)(_))

    def react(fun: Tx => Option[B] => Unit)(implicit tx: Tx): Disposable[Tx] =
      in.react { implicit tx => aOpt =>
        fun(tx)(aOpt.flatMap(f(tx)(_)))
      }
  }

  private[lucre] final class CatVarImpl[Tx, A, B](in: CellView[Tx, Option[A]])(cat: Tx => A => CellView.Var[Tx, Option[B]])
    extends CellView.Var[Tx, Option[B]] {

    def apply()(implicit tx: Tx): Option[B] =
      in().flatMap(cat(tx)(_).apply())

    def update(v: Option[B])(implicit tx: Tx): Unit =
      in().foreach(cat(tx)(_).update(v))

    def react(fun: Tx => Option[B] => Unit)(implicit tx: Tx): Disposable[Tx] =
      in.react { implicit tx => aOpt =>
        fun(tx)(aOpt.flatMap(cat(tx)(_).apply()))
      }
  }

  private final class Const[Tx, A](value: A)
    extends CellView[Tx, A] {

    def react(fun: Tx => A => Unit)(implicit tx: Tx): Disposable[Tx] = Disposable.empty

    def apply()(implicit tx: Tx): A = value
  }

  private[lucre] trait AttrBasic[S <: Sys[S], A, E[~ <: Sys[~]] <: _Expr[~, A]]
    extends CellView[S#Tx, Option[A]] {

    protected def h: stm.Source[S#Tx, Obj.AttrMap[S]]
    protected val key: String

    // implicit protected def companion: Elem.Companion[E]
    implicit protected val tpe: Type.Expr[A, E]

//    implicit protected def classTag: ClassTag[E[S]]

    final type Repr = Option[E[S]] // Expr[S, A]]

    def react(fun: S#Tx => Option[A] => Unit)(implicit tx: S#Tx): Disposable[S#Tx] =
      new AttrMapExprObs[S, A](map = h(), key = key, fun = fun, tx0 = tx)

    def repr(implicit tx: S#Tx): Repr = {
      val opt = h().get(key)
      opt match {
        case Some(v) if v.tpe.typeId == tpe.typeId =>
          val vt = v.asInstanceOf[E[S]]
          val vv = vt match {
            case tpe.Var(vr)  => vr()
            case other        => other
          }
          Some(vv)

        case _ => None
      }
    }

    def apply()(implicit tx: S#Tx): Option[A] = repr.map(_.value)
  }

  final class AttrMapExprObs[S <: Sys[S], A](map: Obj.AttrMap[S], key: String, fun: S#Tx => Option[A] => Unit,
                                             tx0: S#Tx)(implicit tpe: Obj.Type)
    extends MapLikeExprObs[S, A, Obj](map, key, fun, tx0) {

    protected def compareTpe(value: Obj[S]): Boolean =
      value.tpe == tpe
  }

    // XXX TODO --- lot's of overlap with CellViewImpl
  /** N.B.: `tpe` must denote objects that extend `Expr`, otherwise we get class-cast exceptions. */
  abstract class MapLikeExprObs[S <: Sys[S], A, Repr[~ <: Sys[~]] <: Form[~]](
      map: MapLike[S, String, Repr], key: String, fun: S#Tx => Option[A] => Unit, tx0: S#Tx)
    extends Disposable[S#Tx] {

    private[this] val valObs = Ref(null: Disposable[S#Tx])

    protected def compareTpe(in: Repr[S]): Boolean

    private[this] def obsAdded(value: Repr[S])(implicit tx: S#Tx): Unit = {
      val valueT = value.asInstanceOf[_Expr[S, A]]
      valueAdded(valueT)
      // XXX TODO -- if we moved this into `valueAdded`, the contract
      // could be that initially the view is updated
      val now0 = valueT.value
      fun(tx)(Some(now0))
    }

    @inline
    private[this] def obsRemoved()(implicit tx: S#Tx): Unit =
      if (valueRemoved()) fun(tx)(None)

    private[this] val mapObs = map.changed.react { implicit tx => u =>
      u.changes.foreach {
        case Obj.AttrAdded   (`key`, value) if compareTpe(value) => obsAdded  (value)
        case Obj.AttrRemoved (`key`, value) if compareTpe(value) => obsRemoved()
        case Obj.AttrReplaced(`key`, before, now) =>
          if      (compareTpe(now    )) obsAdded(now)
          else if (compareTpe(before )) obsRemoved()
        case _ =>
      }
    } (tx0)

    map.get(key)(tx0).foreach { value =>
      if (compareTpe(value)) valueAdded(value.asInstanceOf[_ExprLike[S, A]])(tx0)
    }

    private[this] def valueAdded(value: _ExprLike[S, A])(implicit tx: S#Tx): Unit = {
      val res = value.changed.react { implicit tx => {
        case Change(_, now) =>
          fun(tx)(Some(now))
        //            val opt = mapUpdate(ch)
        //            if (opt.isDefined) fun(tx)(opt)
        case _ =>  // XXX TODO -- should we ask for expr.value ?
      }}
      val v = valObs.swap(res)(tx.peer)
      if (v != null) v.dispose()
    }

    private[this] def valueRemoved()(implicit tx: S#Tx): Boolean = {
      val v   = valObs.swap(null)(tx.peer)
      val res = v != null
      if (res) v.dispose()
      res
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      valueRemoved()
      mapObs.dispose()
    }
  }

  private[lucre] abstract class AttrImpl[S <: Sys[S], A, E[~ <: Sys[~]] <: _Expr[~, A]](
      protected val h: stm.Source[S#Tx, Obj.AttrMap[S]],
      protected val key: String)(implicit val tpe: Type.Expr[A, E]
  )
    extends AttrBasic[S, A, E] with CellView.VarR[S, Option[A]] {

    final type EVar[~ <: Sys[~]] = tpe.Var[~]

    // ---- abstract ----

    protected def putImpl   (map: Obj.AttrMap[S], value: E[S])(implicit tx: S#Tx): Unit
    protected def removeImpl(map: Obj.AttrMap[S]             )(implicit tx: S#Tx): Unit
    protected def updateVarImpl(vr: EVar[S]     , value: E[S])(implicit tx: S#Tx): Unit

    // ---- impl ----

    final def serializer: Serializer[S#Tx, S#Acc, Repr] = {
      implicit val exSer: Serializer[S#Tx, S#Acc, E[S]] = tpe.serializer[S]
      Serializer.option[S#Tx, S#Acc, E[S]]
    }

    protected final def mapUpdate(ch: Change[A]): Option[A] = if (ch.isSignificant) Some(ch.now) else None

    private def overwrite(map: Obj.AttrMap[S], value: E[S])(implicit tx: S#Tx): Unit = {
      val aObj = tpe.Var.unapply[S](value).getOrElse(tpe.newVar(value))
      putImpl(map, aObj)
    }

    def repr_=(value: Repr)(implicit tx: S#Tx): Unit = value.fold[Unit] {
      val map = h()
      removeImpl(map)
    } { ex =>
      val map = h()
      val opt = map.get(key)
      opt match {
        case Some(v) if v.tpe.typeId == tpe.typeId =>
          val vt = v.asInstanceOf[E[S]]
          vt match {
            case tpe.Var(vr) => updateVarImpl(vr, ex)
            case _ => overwrite(map, ex)
          }

        case _ => overwrite(map,ex)
      }
    }

    def lift(value: Option[A])(implicit tx: S#Tx): Repr =
      value.map(v => tpe.newConst[S](v))

    def update(v: Option[A])(implicit tx: S#Tx): Unit = repr_=(lift(v))
  }

  // plain mutating implementation (no undo support)
  private final class PlainAttrImpl[S <: Sys[S], A, E[~ <: Sys[~]] <: _Expr[~, A]](
      h: stm.Source[S#Tx, Obj.AttrMap[S]], key: String)(implicit tpe: Type.Expr[A, E])
    extends AttrImpl[S, A, E](h, key) {

    protected def putImpl(map: AttrMap[S], value: E[S])(implicit tx: S#Tx): Unit =
      map.put(key, value)

    protected def removeImpl(map: AttrMap[S])(implicit tx: S#Tx): Unit =
      map.remove(key)

    protected def updateVarImpl(vr: EVar[S], value: E[S])(implicit tx: S#Tx): Unit =
      vr() = value    // IntelliJ highlight bug
  }

  // adding optional undo support (when present)
  private final class UndoAttrImpl[S <: Sys[S], A, E[~ <: Sys[~]] <: _Expr[~, A]](
      h: stm.Source[S#Tx, Obj.AttrMap[S]], key: String)(implicit tpe: Type.Expr[A, E])
    extends AttrImpl[S, A, E](h, key) {

    protected def putImpl(map: AttrMap[S], value: E[S])(implicit tx: S#Tx): Unit =
      EditAttrMap.put(map, key, value)

    protected def removeImpl(map: AttrMap[S])(implicit tx: S#Tx): Unit =
      EditAttrMap.remove(map, key)

    protected def updateVarImpl(vr: EVar[S], value: E[S])(implicit tx: S#Tx): Unit =
      EditExprVar.apply[S, A, E](vr, value)  // IntelliJ highlight bug
  }
}
