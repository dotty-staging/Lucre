/*
 *  Artifact.scala
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

import de.sciss.file._
import de.sciss.lucre.artifact.{ArtifactLocation, Artifact => _Artifact}
import de.sciss.lucre.aux.Aux
import de.sciss.lucre.edit.EditAttrMap
import de.sciss.lucre.expr.impl.CellViewImpl.AttrMapExprObs
import de.sciss.lucre.expr.{CellView, Context, IExpr}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Obj.AttrMap
import de.sciss.lucre.stm.{Disposable, Sys}
import de.sciss.serial.{DataInput, Serializer}

import scala.util.{Failure, Success, Try}

object Artifact {
  private lazy val _init: Unit = Aux.addFactory(Bridge)

  def init(): Unit = _init

  private final object Bridge extends Obj.Bridge[File] with Aux.Factory {
    final val id = 2000

    type Repr[S <: Sys[S]] = _Artifact[S]

    def readIdentifiedAux(in: DataInput): Aux = this

    def cellView[S <: Sys[S]](obj: stm.Obj[S], key: String)(implicit tx: S#Tx): CellView.Var[S#Tx, Option[File]] =
      new ObjCellViewImpl(tx.newHandle(obj.attr), key = key)

    // Note: Artifact does not expose an implicit `Obj.Bridge[File]`, for now, so currently
    // we cannot write `"key".attr[File]` anyway!
    def contextCellView[S <: Sys[S]](key: String)(implicit tx: S#Tx, context: Context[S]): CellView[S#Tx, Option[File]] = {
      println(s"Warning: Artifact.cellView($key) not yet implemented for context. Using fall-back")
      context.selfOption.fold(CellView.const[S, Option[File]](None))(cellView(_, key))
    }

    def cellValue[S <: Sys[S]](obj: stm.Obj[S], key: String)(implicit tx: S#Tx): Option[File] =
      obj.attr.$[_Artifact](key).map(_.value)
  }

  private def tryRelativize[S <: Sys[S]](loc: ArtifactLocation[S], f: File)(implicit tx: S#Tx): Try[_Artifact.Child] =
    Try(_Artifact.relativize(loc.directory, f))

  private def defaultLocation[S <: Sys[S]](f: File)(implicit tx: S#Tx): ArtifactLocation[S] =
    ArtifactLocation.newVar(f.absolute.parent)

  private def makeArtifact[S <: Sys[S]](loc: ArtifactLocation[S], f: File)(implicit tx: S#Tx): _Artifact[S] = {
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

    type Repr = Option[_Artifact[S]]

    def serializer: Serializer[S#Tx, S#Acc, Repr] = Serializer.option

    def repr(implicit tx: S#Tx): Repr =
      attr.$[_Artifact](key)

    private def putImpl(map: AttrMap[S], value: _Artifact[S])(implicit tx: S#Tx): Unit =
      EditAttrMap.put(map, key, value)

    private def removeImpl(map: AttrMap[S])(implicit tx: S#Tx): Unit =
      EditAttrMap.remove(map, key)

    def repr_=(value: Repr)(implicit tx: S#Tx): Unit =
      value match {
        case Some(a)  => putImpl(attr, a)
        case None     => removeImpl(attr)
      }

    def lift(v: Option[File])(implicit tx: S#Tx): Repr =
      v match {
        case Some(f) if f.path.nonEmpty =>
          val loc = repr.fold[ArtifactLocation[S]](defaultLocation(f))(_.location)
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
                case Success(child) => am.child = child
                case Failure(_) => fallback()
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
  extends Attr.WithDefault[File] {

  type Repr[S <: Sys[S]] = IExpr[S, File]

  protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
    val defaultEx: Repr[S] = default.expand[S]
    val attrView = Attr.resolveNested(key)
    import ctx.targets
    new Attr.WithDefault.Expanded[S, File](attrView, defaultEx, tx)
  }

  def update(in: Ex[File]): Control = Attr.Update (in, key)
  def set   (in: Ex[File]): Act     = Attr.Set    (in, key)

  implicit def bridge: Obj.Bridge[File] = Artifact.Bridge

  def aux: List[Aux] = Nil
}
