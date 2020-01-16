/*
 *  ExSeq.scala
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

package de.sciss.lucre.expr

import de.sciss.lucre.adjunct.{Adjunct, ProductWithAdjuncts}
import de.sciss.lucre.event.impl.IChangeEventImpl
import de.sciss.lucre.event.{IChangeEvent, IPull, ITargets}
import de.sciss.lucre.expr
import de.sciss.lucre.expr.graph.impl.{ExpandedMapSeqIn, MappedIExpr}
import de.sciss.lucre.expr.graph.{Ex, It, Obj}
import de.sciss.lucre.stm.Sys

object ExSeq {
  private final class Expanded[S <: Sys[S], A](elems: Seq[IExpr[S, A]])(implicit protected val targets: ITargets[S])
    extends IExpr[S, Seq[A]] with IChangeEventImpl[S, Seq[A]] {

    def init()(implicit tx: S#Tx): this.type = {
      elems.foreach { in =>
        in.changed ---> changed
      }
      this
    }

    def value(implicit tx: S#Tx): Seq[A] = elems.map(_.value)

    def dispose()(implicit tx: S#Tx): Unit = {
      elems.foreach { in =>
        in.changed -/-> changed
      }
    }

    def changed: IChangeEvent[S, Seq[A]] = this

    private[lucre] def pullChange(pull: IPull[S])(implicit tx: S#Tx, phase: IPull.Phase): Seq[A] = {
      val b = Seq.newBuilder[A]
      b.sizeHint(elems)
      elems.foreach { in =>
        val v = pull.expr(in)
        b += v
      }
      b.result()
    }
  }

  private final class CountExpanded[S <: Sys[S], A](in: IExpr[S, Seq[A]], it: It.Expanded[S, A],
                                                   fun: Ex[Boolean], tx0: S#Tx)
                                                  (implicit targets: ITargets[S], ctx: Context[S])
    extends ExpandedMapSeqIn[S, A, Boolean, Int](in, it, fun, tx0) {

    override def toString: String = s"$in.count($fun)"

    protected def emptyOut: Int = 0

    protected def buildResult(inV: Seq[A], tuples: Tuples)(elem: IExpr[S, Boolean] => Boolean)
                             (implicit tx: S#Tx): Int = {
      assert (tuples.size == inV.size)
      val iterator  = inV.iterator zip tuples.iterator
      var res       = 0
      while (iterator.hasNext) {
        val (vn, (f, _)) = iterator.next()
        it.setValue(vn)
        val funV = elem(f)
        if (funV) res += 1
      }
      res
    }
  }

  final case class Count[A] private (in: Ex[Seq[A]], it: It[A], p: Ex[Boolean])
    extends Ex[Int] {

    type Repr[S <: Sys[S]] = IExpr[S, Int]

    override def productPrefix: String = s"ExSeq$$Count" // serialization

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      val inEx = in.expand[S]
      val itEx = it.expand[S]
      import ctx.targets
      new CountExpanded[S, A](inEx, itEx, p, tx)
    }
  }
  
  private final class DropWhileExpanded[S <: Sys[S], A](in: IExpr[S, Seq[A]], it: It.Expanded[S, A],
                                                         fun: Ex[Boolean], tx0: S#Tx)
                                                        (implicit targets: ITargets[S], ctx: Context[S])
    extends ExpandedMapSeqIn[S, A, Boolean, Seq[A]](in, it, fun, tx0) {

    override def toString: String = s"$in.dropWhile($fun)"

    protected def emptyOut: Seq[A] = Nil

    protected def buildResult(inV: Seq[A], tuples: Tuples)(elem: IExpr[S, Boolean] => Boolean)
                             (implicit tx: S#Tx): Seq[A] = {
      assert (tuples.size == inV.size)
      val iterator  = inV.iterator zip tuples.iterator
      while (iterator.hasNext) {
        val (vn, (f, _)) = iterator.next()
        it.setValue(vn)
        val funV = elem(f)
        if (!funV) {
          val b = Seq.newBuilder[A]
          b += vn
          while (iterator.hasNext) {
            val (vn, _) = iterator.next()
            b += vn
          }
          return b.result()
        }
      }
      Nil
    }
  }

  final case class DropWhile[A] private (in: Ex[Seq[A]], it: It[A], p: Ex[Boolean])
    extends Ex[Seq[A]] {

    type Repr[S <: Sys[S]] = IExpr[S, Seq[A]]

    override def productPrefix: String = s"ExSeq$$DropWhile" // serialization

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      val inEx = in.expand[S]
      val itEx = it.expand[S]
      import ctx.targets
      new DropWhileExpanded[S, A](inEx, itEx, p, tx)
    }
  }
  
  private final class ExistsExpanded[S <: Sys[S], A](in: IExpr[S, Seq[A]], it: It.Expanded[S, A],
                                                   fun: Ex[Boolean], tx0: S#Tx)
                                                  (implicit targets: ITargets[S], ctx: Context[S])
    extends ExpandedMapSeqIn[S, A, Boolean, Boolean](in, it, fun, tx0) {

    override def toString: String = s"$in.exists($fun)"

    protected def emptyOut: Boolean = false

    protected def buildResult(inV: Seq[A], tuples: Tuples)(elem: IExpr[S, Boolean] => Boolean)
                             (implicit tx: S#Tx): Boolean = {
      assert (tuples.size == inV.size)
      val iterator = inV.iterator zip tuples.iterator
      while (iterator.hasNext) {
        val (vn, (f, _)) = iterator.next()
        it.setValue(vn)
        val funV = elem(f)
        if (funV) return true
      }
      false
    }
  }

  final case class Exists[A] private (in: Ex[Seq[A]], it: It[A], p: Ex[Boolean])
    extends Ex[Boolean] {

    type Repr[S <: Sys[S]] = IExpr[S, Boolean]

    override def productPrefix: String = s"ExSeq$$Exists" // serialization

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      val inEx = in.expand[S]
      val itEx = it.expand[S]
      import ctx.targets
      new ExistsExpanded[S, A](inEx, itEx, p, tx)
    }
  }

  private final class ForallExpanded[S <: Sys[S], A](in: IExpr[S, Seq[A]], it: It.Expanded[S, A],
                                                     fun: Ex[Boolean], tx0: S#Tx)
                                                    (implicit targets: ITargets[S], ctx: Context[S])
    extends ExpandedMapSeqIn[S, A, Boolean, Boolean](in, it, fun, tx0) {

    override def toString: String = s"$in.forall($fun)"

    protected def emptyOut: Boolean = true

    protected def buildResult(inV: Seq[A], tuples: Tuples)(elem: IExpr[S, Boolean] => Boolean)
                             (implicit tx: S#Tx): Boolean = {
      assert (tuples.size == inV.size)
      val iterator = inV.iterator zip tuples.iterator
      while (iterator.hasNext) {
        val (vn, (f, _)) = iterator.next()
        it.setValue(vn)
        val funV = elem(f)
        if (!funV) return false
      }
      true
    }
  }

  private final class FilterExpanded[S <: Sys[S], A](in: IExpr[S, Seq[A]], it: It.Expanded[S, A],
                                                        fun: Ex[Boolean], tx0: S#Tx)
                                                       (implicit targets: ITargets[S], ctx: Context[S])
    extends ExpandedMapSeqIn[S, A, Boolean, Seq[A]](in, it, fun, tx0) {

    override def toString: String = s"$in.filter($fun)"

    protected def emptyOut: Seq[A] = Nil

    protected def buildResult(inV: Seq[A], tuples: Tuples)(elem: IExpr[S, Boolean] => Boolean)
                             (implicit tx: S#Tx): Seq[A] = {
      assert (tuples.size == inV.size)
      val iterator  = inV.iterator zip tuples.iterator
      val b         = Seq.newBuilder[A]
      while (iterator.hasNext) {
        val (vn, (f, _)) = iterator.next()
        it.setValue(vn)
        val funV = elem(f)
        if (funV) b += vn
      }
      b.result()
    }
  }

  final case class Filter[A] private (in: Ex[Seq[A]], it: It[A], p: Ex[Boolean])
    extends Ex[Seq[A]] {

    type Repr[S <: Sys[S]] = IExpr[S, Seq[A]]

    override def productPrefix: String = s"ExSeq$$Filter" // serialization

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      val inEx = in.expand[S]
      val itEx = it.expand[S]
      import ctx.targets
      new FilterExpanded[S, A](inEx, itEx, p, tx)
    }
  }

  private final class FilterNotExpanded[S <: Sys[S], A](in: IExpr[S, Seq[A]], it: It.Expanded[S, A],
                                                     fun: Ex[Boolean], tx0: S#Tx)
                                                    (implicit targets: ITargets[S], ctx: Context[S])
    extends ExpandedMapSeqIn[S, A, Boolean, Seq[A]](in, it, fun, tx0) {

    override def toString: String = s"$in.filterNot($fun)"

    protected def emptyOut: Seq[A] = Nil

    protected def buildResult(inV: Seq[A], tuples: Tuples)(elem: IExpr[S, Boolean] => Boolean)
                             (implicit tx: S#Tx): Seq[A] = {
      assert (tuples.size == inV.size)
      val iterator  = inV.iterator zip tuples.iterator
      val b         = Seq.newBuilder[A]
      while (iterator.hasNext) {
        val (vn, (f, _)) = iterator.next()
        it.setValue(vn)
        val funV = elem(f)
        if (!funV) b += vn
      }
      b.result()
    }
  }

  final case class FilterNot[A] private (in: Ex[Seq[A]], it: It[A], p: Ex[Boolean])
    extends Ex[Seq[A]] {

    type Repr[S <: Sys[S]] = IExpr[S, Seq[A]]

    override def productPrefix: String = s"ExSeq$$FilterNot" // serialization

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      val inEx = in.expand[S]
      val itEx = it.expand[S]
      import ctx.targets
      new FilterNotExpanded[S, A](inEx, itEx, p, tx)
    }
  }

  final case class Forall[A] private (in: Ex[Seq[A]], it: It[A], p: Ex[Boolean])
    extends Ex[Boolean] {

    type Repr[S <: Sys[S]] = IExpr[S, Boolean]

    override def productPrefix: String = s"ExSeq$$Forall" // serialization

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      val inEx = in.expand[S]
      val itEx = it.expand[S]
      import ctx.targets
      new ForallExpanded[S, A](inEx, itEx, p, tx)
    }
  }

  private final class FindExpanded[S <: Sys[S], A](in: IExpr[S, Seq[A]], it: It.Expanded[S, A],
                                                   fun: Ex[Boolean], tx0: S#Tx)
                                                  (implicit targets: ITargets[S], ctx: Context[S])
    extends ExpandedMapSeqIn[S, A, Boolean, Option[A]](in, it, fun, tx0) {

    override def toString: String = s"$in.find($fun)"

    protected def emptyOut: Option[A] = None

    protected def buildResult(inV: Seq[A], tuples: Tuples)(elem: IExpr[S, Boolean] => Boolean)
                             (implicit tx: S#Tx): Option[A] = {
      assert (tuples.size == inV.size)
      val iterator  = inV.iterator zip tuples.iterator
      while (iterator.hasNext) {
        val (vn, (f, _)) = iterator.next()
        it.setValue(vn)
        val funV = elem(f)
        if (funV) return Some(vn)
      }
      None
    }
  }

  final case class Find[A] private (in: Ex[Seq[A]], it: It[A], p: Ex[Boolean])
    extends Ex[Option[A]] {

    type Repr[S <: Sys[S]] = IExpr[S, Option[A]]

    override def productPrefix: String = s"ExSeq$$Find" // serialization

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      val inEx = in.expand[S]
      val itEx = it.expand[S]
      import ctx.targets
      new FindExpanded[S, A](inEx, itEx, p, tx)
    }
  }

  private final class FindLastExpanded[S <: Sys[S], A](in: IExpr[S, Seq[A]], it: It.Expanded[S, A],
                                                   fun: Ex[Boolean], tx0: S#Tx)
                                                  (implicit targets: ITargets[S], ctx: Context[S])
    extends ExpandedMapSeqIn[S, A, Boolean, Option[A]](in, it, fun, tx0) {

    override def toString: String = s"$in.findLast($fun)"

    protected def emptyOut: Option[A] = None

    protected def buildResult(inV: Seq[A], tuples: Tuples)(elem: IExpr[S, Boolean] => Boolean)
                             (implicit tx: S#Tx): Option[A] = {
      assert (tuples.size == inV.size)
      val iterator  = inV.reverseIterator zip tuples.reverseIterator
      while (iterator.hasNext) {
        val (vn, (f, _)) = iterator.next()
        it.setValue(vn)
        val funV = elem(f)
        if (funV) return Some(vn)
      }
      None
    }
  }

  final case class FindLast[A] private (in: Ex[Seq[A]], it: It[A], p: Ex[Boolean])
    extends Ex[Option[A]] {

    type Repr[S <: Sys[S]] = IExpr[S, Option[A]]

    override def productPrefix: String = s"ExSeq$$FindLast" // serialization

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      val inEx = in.expand[S]
      val itEx = it.expand[S]
      import ctx.targets
      new FindLastExpanded[S, A](inEx, itEx, p, tx)
    }
  }
  
  private final class IndexWhereExpanded[S <: Sys[S], A](in: IExpr[S, Seq[A]], it: It.Expanded[S, A],
                                                       fun: Ex[Boolean], tx0: S#Tx)
                                                      (implicit targets: ITargets[S], ctx: Context[S])
    extends ExpandedMapSeqIn[S, A, Boolean, Int](in, it, fun, tx0) {

    override def toString: String = s"$in.indexWhere($fun)"

    protected def emptyOut: Int = -1

    protected def buildResult(inV: Seq[A], tuples: Tuples)(elem: IExpr[S, Boolean] => Boolean)
                             (implicit tx: S#Tx): Int = {
      assert (tuples.size == inV.size)
      val iterator  = inV.iterator zip tuples.iterator
      var res       = 0
      while (iterator.hasNext) {
        val (vn, (f, _)) = iterator.next()
        it.setValue(vn)
        val funV = elem(f)
        if (funV) return res
        res += 1
      }
      -1
    }
  }

  final case class IndexWhere[A] private (in: Ex[Seq[A]], it: It[A], p: Ex[Boolean])
    extends Ex[Int] {

    type Repr[S <: Sys[S]] = IExpr[S, Int]

    override def productPrefix: String = s"ExSeq$$IndexWhere" // serialization

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      val inEx = in.expand[S]
      val itEx = it.expand[S]
      import ctx.targets
      new IndexWhereExpanded[S, A](inEx, itEx, p, tx)
    }
  }

  // XXX TODO --- we should use cell-views instead, because this way we won't notice
  // changes to the value representation (e.g. a `StringObj.Var` contents change)
  private final class SelectExpanded[S <: Sys[S], A](in: IExpr[S, Seq[Obj]], tx0: S#Tx)
                                                    (implicit targets: ITargets[S], bridge: Obj.Bridge[A])
    extends MappedIExpr[S, Seq[Obj], Seq[A]](in, tx0) {

    protected def mapValue(inValue: Seq[Obj])(implicit tx: S#Tx): Seq[A] =
      inValue.flatMap(_.peer.flatMap(bridge.tryParseObj(_)))
  }

  final case class Select[A] private (in: Ex[Seq[Obj]])(implicit bridge: Obj.Bridge[A])
    extends Ex[Seq[A]] with ProductWithAdjuncts {

    type Repr[S <: Sys[S]] = IExpr[S, Seq[A]]

    override def productPrefix: String = s"ExSeq$$Select" // serialization

    def adjuncts: List[Adjunct] = bridge :: Nil

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      val inEx = in.expand[S]
      import ctx.targets
      new SelectExpanded[S, A](inEx, tx)
    }
  }

  // XXX TODO --- we should use cell-views instead, because this way we won't notice
  // changes to the value representation (e.g. a `StringObj.Var` contents change)
  private final class SelectFirstExpanded[S <: Sys[S], A](in: IExpr[S, Seq[Obj]], tx0: S#Tx)
                                                    (implicit targets: ITargets[S], bridge: Obj.Bridge[A])
    extends MappedIExpr[S, Seq[Obj], Option[A]](in, tx0) {

    protected def mapValue(inValue: Seq[Obj])(implicit tx: S#Tx): Option[A] = {
      val it = inValue.iterator.flatMap(_.peer.flatMap(bridge.tryParseObj(_)))
      if (it.hasNext) Some(it.next()) else None
    }
  }

  final case class SelectFirst[A] private (in: Ex[Seq[Obj]])(implicit bridge: Obj.Bridge[A])
    extends Ex[Option[A]] with ProductWithAdjuncts {

    type Repr[S <: Sys[S]] = IExpr[S, Option[A]]

    override def productPrefix: String = s"ExSeq$$SelectFirst" // serialization

    def adjuncts: List[Adjunct] = bridge :: Nil

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      val inEx = in.expand[S]
      import ctx.targets
      new SelectFirstExpanded[S, A](inEx, tx)
    }
  }

  private final class TakeWhileExpanded[S <: Sys[S], A](in: IExpr[S, Seq[A]], it: It.Expanded[S, A],
                                                        fun: Ex[Boolean], tx0: S#Tx)
                                                       (implicit targets: ITargets[S], ctx: Context[S])
    extends ExpandedMapSeqIn[S, A, Boolean, Seq[A]](in, it, fun, tx0) {

    override def toString: String = s"$in.takeWhile($fun)"

    protected def emptyOut: Seq[A] = Nil

    protected def buildResult(inV: Seq[A], tuples: Tuples)(elem: IExpr[S, Boolean] => Boolean)
                             (implicit tx: S#Tx): Seq[A] = {
      assert (tuples.size == inV.size)
      val iterator  = inV.iterator zip tuples.iterator
      val b = Seq.newBuilder[A]
      while (iterator.hasNext) {
        val (vn, (f, _)) = iterator.next()
        it.setValue(vn)
        val funV = elem(f)
        if (!funV) {
          return b.result()
        }
        b += vn
      }
      b.result()
    }
  }

  final case class TakeWhile[A] private (in: Ex[Seq[A]], it: It[A], p: Ex[Boolean])
    extends Ex[Seq[A]] {

    type Repr[S <: Sys[S]] = IExpr[S, Seq[A]]

    override def productPrefix: String = s"ExSeq$$TakeWhile" // serialization

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      val inEx = in.expand[S]
      val itEx = it.expand[S]
      import ctx.targets
      new TakeWhileExpanded[S, A](inEx, itEx, p, tx)
    }
  }

}
final case class ExSeq[A](elems: Ex[A]*) extends Ex[Seq[A]] {

  type Repr[S <: Sys[S]] = IExpr[S, Seq[A]]

  private def simpleString: String = {
    val xs = elems.iterator.take(5).toList
    val es = if (xs.lengthCompare(5) == 0) xs.init.mkString("", ", ", ", ...")
    else xs.mkString(", ")
    s"ExSeq($es)"
  }

  override def toString: String = simpleString

  protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
    import ctx.targets
    val elemsEx: Seq[IExpr[S, A]] = elems.iterator.map(_.expand[S]).toList
    new expr.ExSeq.Expanded(elemsEx).init()
  }
}
