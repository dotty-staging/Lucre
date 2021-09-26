/*
 *  ArtifactLocation.scala
 *  (Lucre 4)
 *
 *  Copyright (c) 2009-2021 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr.graph

import de.sciss.lucre.Obj.AttrMap
import de.sciss.lucre.edit.{EditAttrMap, EditExprVar}
import de.sciss.lucre.expr.ExElem.{ProductReader, RefMapIn}
import de.sciss.lucre.expr.graph.impl.AbstractCtxCellView
import de.sciss.lucre.expr.impl.CellViewImpl.AttrMapExprObs
import de.sciss.lucre.expr.{CellView, Context}
import de.sciss.lucre.{Adjunct, Disposable, IExpr, ProductWithAdjuncts, Source, Txn, ArtifactLocation => _ArtifactLocation, Obj => LObj}
import de.sciss.serial.DataInput

import java.net.URI

object ArtifactLocation extends ProductReader[ArtifactLocation] {
  private lazy val _init: Unit = Adjunct.addFactory(Bridge)

  def init(): Unit = _init

  private object Bridge extends Obj.Bridge[URI] with Adjunct.Factory {
    final val id = 2003

    def readIdentifiedAdjunct(in: DataInput): Adjunct = this

    def cellView[T <: Txn[T]](obj: LObj[T], key: String)(implicit tx: T): CellView.Var[T, Option[URI]] =
      new ObjCellViewImpl(tx.newHandle(obj.attr), key = key)

    def contextCellView[T <: Txn[T]](key: String)(implicit tx: T, context: Context[T]): CellView[T, Option[URI]] =
      new AbstractCtxCellView[T, URI](context.attr, key) {
        protected def tryParseValue(value: Any)(implicit tx: T): Option[URI] = value match {
          case f: URI   => Some(f)
          case _        => None
        }

        protected def tryParseObj(obj: LObj[T])(implicit tx: T): Option[URI] = obj match {
          case a: _ArtifactLocation[T]  => Some(a .value)
          case _                        => None
        }
      }

    def cellValue[T <: Txn[T]](obj: LObj[T], key: String)(implicit tx: T): Option[URI] =
      obj.attr.$[_ArtifactLocation](key).map(_.value)

    def tryParseObj[T <: Txn[T]](obj: LObj[T])(implicit tx: T): Option[URI] = obj match {
      case a: _ArtifactLocation[T]  => Some(a.value)
      case _                        => None
    }
  }

  private final class ObjCellViewImpl[T <: Txn[T]](attrH: Source[T, AttrMap[T]], key: String)
    extends CellView.Var[T, Option[URI]] {

    private def attr(implicit tx: T): AttrMap[T] = attrH()

    private type Repr = Option[_ArtifactLocation[T]]

    private def repr(implicit tx: T): Repr =
      attr.$[_ArtifactLocation](key)

    private def putImpl(map: AttrMap[T], value: _ArtifactLocation[T])(implicit tx: T): Unit =
      EditAttrMap.put(map, key, value)

    private def removeImpl(map: AttrMap[T])(implicit tx: T): Unit =
      EditAttrMap.remove(map, key)

    private def repr_=(value: Repr)(implicit tx: T): Unit =
      value match {
        case Some(a)  => putImpl(attr, a)
        case None     => removeImpl(attr)
      }

    private def lift(v: Option[URI])(implicit tx: T): Repr =
      v match {
        case Some(f) if f.getPath.nonEmpty =>
          val loc = _ArtifactLocation.newVar[T](f)
          Some(loc)

        case _ => None
      }

    def apply()(implicit tx: T): Option[URI] = repr.map(_.value)

    def update(v: Option[URI])(implicit tx: T): Unit = {
      def fallback(): Unit = repr_=(lift(v))

      v match {
        case Some(f) if f.getPath.nonEmpty =>
          repr match {
            case Some(_ArtifactLocation.Var(am)) =>
              EditExprVar[T, URI, _ArtifactLocation](am, f)
            case _ => fallback()
          }
        case _ => fallback()
      }
    }

    def react(fun: T => Option[URI] => Unit)(implicit tx: T): Disposable[T] =
      new AttrMapExprObs[T, URI](map = attr, key = key, fun = fun, tx0 = tx)(_ArtifactLocation)
  }

  override def read(in: RefMapIn, key: String, arity: Int, adj: Int): ArtifactLocation = {
    require (arity == 2 && adj == 0)
    val _key      = in.readString()
    val _default  = in.readEx[URI]()
    new ArtifactLocation(_key, _default)
  }
}
final case class ArtifactLocation(key: String, default: Ex[URI] = new URI(null, null, null))
  extends Attr.WithDefault[URI] with ProductWithAdjuncts {

  type Repr[T <: Txn[T]] = IExpr[T, URI]

  protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
    val defaultEx: Repr[T] = default.expand[T]
    val attrView = Attr.resolveNested(key)
    import ctx.targets
    new Attr.WithDefault.Expanded[T, URI](key, attrView, defaultEx, tx)
  }

  def update      (in: Ex[URI])         : Unit  = Attr.Update       (in, key)
  def updateOption(in: Ex[Option[URI]]) : Unit  = Attr.UpdateOption (in, key)
  def set         (in: Ex[URI])         : Act   = Attr.Set          (in, key)

  implicit private def bridge: Obj.Bridge[URI] = ArtifactLocation.Bridge

  def adjuncts: List[Adjunct] = Nil
}
