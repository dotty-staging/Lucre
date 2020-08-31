/*
 *  File.scala
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

import java.io.{File => _File}

import de.sciss.equal.Implicits._
import de.sciss.lucre.event.impl.IChangeGenerator
import de.sciss.lucre.event.{IChangeEvent, IPull, ITargets}
import de.sciss.lucre.expr.impl.IActionImpl
import de.sciss.lucre.expr.{Context, IAction, IExpr}
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.model.Change

import scala.concurrent.stm.Ref

object File {
  private final class MkDirExpanded[S <: Sys[S]](f: IExpr[S, _File])
    extends IActionImpl[S] {

    def executeAction()(implicit tx: S#Tx): Unit = {
      val fv = f.value
      tx.afterCommit(fv.mkdirs()) // plural! we want to ensure parent directories are created, too
    }
  }

  final case class MkDir(f: Ex[_File]) extends Act {
    override def productPrefix: String = s"File$$MkDir"  // serialization

    type Repr[S <: Sys[S]] = IAction[S]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] =
      new MkDirExpanded(f.expand[S])
  }

  private final class DeleteExpanded[S <: Sys[S]](f: IExpr[S, _File])
    extends IActionImpl[S] {

    def executeAction()(implicit tx: S#Tx): Unit = {
      val fv = f.value
      tx.afterCommit(fv.delete())
    }
  }

  final case class Delete(f: Ex[_File]) extends Act {
    override def productPrefix: String = s"File$$Delete"  // serialization

    type Repr[S <: Sys[S]] = IAction[S]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] =
      new DeleteExpanded(f.expand[S])
  }

  private final class ListExpanded[S <: Sys[S]](dir: IExpr[S, _File])(implicit protected val targets: ITargets[S])
    extends IExpr[S, Seq[_File]] with IActionImpl[S] with IChangeGenerator[S, Seq[_File]] {

    private[this] val ref = Ref(Seq.empty[_File])

    def value(implicit tx: S#Tx): Seq[_File] = ref()

    override def changed: IChangeEvent[S, Seq[_File]] = this

    private[lucre] def pullChange(pull: IPull[S])(implicit tx: S#Tx, phase: IPull.Phase): Seq[_File] =
      pull.resolveExpr(this)

    def executeAction()(implicit tx: S#Tx): Unit = {
      val dv      = dir.value
      val arr     = dv.listFiles()
      val now     = if (arr == null) Nil else arr.toSeq
      val before  = ref.swap(now)
      if (before !== now) {
        fire(Change(before, now))
      }
    }
  }

  final case class List(dir: Ex[_File]) extends Ex[Seq[_File]] with Act {
    override def productPrefix: String = s"File$$List"  // serialization

    type Repr[S <: Sys[S]] = IExpr[S, Seq[_File]] with IAction[S]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new ListExpanded(dir.expand[S])
    }
  }
}
