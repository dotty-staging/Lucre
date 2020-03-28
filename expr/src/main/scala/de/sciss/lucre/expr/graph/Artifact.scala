/*
 *  Artifact.scala
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

package de.sciss.lucre.expr.graph

import de.sciss.file._
import de.sciss.lucre.artifact.{Artifact => _Artifact, ArtifactLocation => _ArtifactLocation}
import de.sciss.lucre.adjunct.{Adjunct, ProductWithAdjuncts}
import de.sciss.lucre.edit.{EditArtifact, EditAttrMap}
import de.sciss.lucre.expr.graph.impl.AbstractCtxCellView
import de.sciss.lucre.expr.impl.CellViewImpl.AttrMapExprObs
import de.sciss.lucre.expr.{CellView, Context, IExpr}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Obj.AttrMap
import de.sciss.lucre.stm.{Disposable, Sys}
import de.sciss.serial.DataInput

import scala.util.{Failure, Success, Try}

object Artifact {
  private lazy val _init: Unit = Adjunct.addFactory(Bridge)

  def init(): Unit = _init

  private final object Bridge extends Obj.Bridge[File] with Adjunct.Factory {
    final val id = 2000

    def readIdentifiedAdjunct(in: DataInput): Adjunct = this

    def cellView[S <: Sys[S]](obj: stm.Obj[S], key: String)(implicit tx: S#Tx): CellView.Var[S#Tx, Option[File]] =
      new ObjCellViewImpl(tx.newHandle(obj.attr), key = key)

    def contextCellView[S <: Sys[S]](key: String)(implicit tx: S#Tx, context: Context[S]): CellView[S#Tx, Option[File]] =
      new AbstractCtxCellView[S, File](context.attr, key) {
        protected def tryParseValue(value: Any)(implicit tx: S#Tx): Option[File] = value match {
          case f: File  => Some(f)
          case _        => None
        }

        protected def tryParseObj(obj: stm.Obj[S])(implicit tx: S#Tx): Option[File] = obj match {
          case a: _Artifact[S] => Some(a .value)
          case _               => None
        }
      }

    def cellValue[S <: Sys[S]](obj: stm.Obj[S], key: String)(implicit tx: S#Tx): Option[File] =
      obj.attr.$[_Artifact](key).map(_.value)

    def tryParseObj[S <: Sys[S]](obj: stm.Obj[S])(implicit tx: S#Tx): Option[File] = obj match {
      case a: _Artifact[S]  => Some(a.value)
      case _                => None
    }
  }

  private def tryRelativize[S <: Sys[S]](loc: _ArtifactLocation[S], f: File)(implicit tx: S#Tx): Try[_Artifact.Child] =
    Try(_Artifact.relativize(loc.directory, f))

  private def defaultLocation[S <: Sys[S]](f: File)(implicit tx: S#Tx): _ArtifactLocation[S] =
    _ArtifactLocation.newVar(f.absolute.parent)

  private def makeArtifact[S <: Sys[S]](loc: _ArtifactLocation[S], f: File)(implicit tx: S#Tx): _Artifact[S] = {
    val art = tryRelativize(loc, f).toOption.fold[_Artifact[S]]({ // Try#fold not available in Scala 2.11
      _Artifact(defaultLocation(f), f)
    }) { child =>
      _Artifact(loc, child)
    }
    art
  }

  private final class ObjCellViewImpl[S <: Sys[S]](attrH: stm.Source[S#Tx, AttrMap[S]], key: String)
    extends CellView.Var[S#Tx, Option[File]] {

    private def attr(implicit tx: S#Tx): AttrMap[S] = attrH()

    private type Repr = Option[_Artifact[S]]

//    def serializer: Serializer[S#Tx, S#Acc, Repr] = Serializer.option

    private def repr(implicit tx: S#Tx): Repr =
      attr.$[_Artifact](key)

    private def putImpl(map: AttrMap[S], value: _Artifact[S])(implicit tx: S#Tx): Unit =
      EditAttrMap.put(map, key, value)

    private def removeImpl(map: AttrMap[S])(implicit tx: S#Tx): Unit =
      EditAttrMap.remove(map, key)

    private def repr_=(value: Repr)(implicit tx: S#Tx): Unit =
      value match {
        case Some(a)  => putImpl(attr, a)
        case None     => removeImpl(attr)
      }

    private def lift(v: Option[File])(implicit tx: S#Tx): Repr =
      v match {
        case Some(f) if f.path.nonEmpty =>
          val loc = repr.fold[_ArtifactLocation[S]](defaultLocation(f))(_.location)
          val art = makeArtifact(loc, f)
          Some(art)

        case _ => None
      }

    def apply()(implicit tx: S#Tx): Option[File] = repr.map(_.value)

    def update(v: Option[File])(implicit tx: S#Tx): Unit = {
      def fallback(): Unit = repr_=(lift(v))

      v match {
        case Some(f) if f.path.nonEmpty =>
          repr match {
            case Some(am: _Artifact.Modifiable[S]) =>
              tryRelativize(am.location, f) match {
                case Success(child) => EditArtifact.updateChild(am, child)
                case Failure(_)     => fallback()
              }
            case _ => fallback()
          }
        case _ => fallback()
      }
    }

    def react(fun: S#Tx => Option[File] => Unit)(implicit tx: S#Tx): Disposable[S#Tx] =
      new AttrMapExprObs[S, File](map = attr, key = key, fun = fun, tx0 = tx)(_Artifact)
  }
}
final case class Artifact(key: String, default: Ex[File] = file(""))
  extends Attr.WithDefault[File] with ProductWithAdjuncts {

  type Repr[S <: Sys[S]] = IExpr[S, File]

  protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
    val defaultEx: Repr[S] = default.expand[S]
    val attrView = Attr.resolveNested(key)
    import ctx.targets
    new Attr.WithDefault.Expanded[S, File](key, attrView, defaultEx, tx)
  }

  def update(in: Ex[File]): Control = Attr.Update (in, key)
  def set   (in: Ex[File]): Act     = Attr.Set    (in, key)

  implicit private def bridge: Obj.Bridge[File] = Artifact.Bridge

  def adjuncts: List[Adjunct] = Nil
}
