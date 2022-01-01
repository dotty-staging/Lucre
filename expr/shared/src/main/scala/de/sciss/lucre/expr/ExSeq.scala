/*
 *  ExSeq.scala
 *  (Lucre 4)
 *
 *  Copyright (c) 2009-2022 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr

import de.sciss.lucre.expr.ExElem.{ProductReader, RefMapIn}
import de.sciss.lucre.expr.graph.impl.{ExpandedMapSeqIn, MappedIExpr}
import de.sciss.lucre.expr.graph.{Ex, It, Obj}
import de.sciss.lucre.impl.IChangeEventImpl
import de.sciss.lucre.{Adjunct, IChangeEvent, IExpr, IPull, ITargets, ProductWithAdjuncts, Txn, expr}

object ExSeq extends ProductReader[ExSeq[_]] {
  private final class Expanded[T <: Txn[T], A](elems: Seq[IExpr[T, A]])(implicit protected val targets: ITargets[T])
    extends IExpr[T, Seq[A]] with IChangeEventImpl[T, Seq[A]] {

    def init()(implicit tx: T): this.type = {
      elems.foreach { in =>
        in.changed ---> changed
      }
      this
    }

    def value(implicit tx: T): Seq[A] = elems.map(_.value)

    def dispose()(implicit tx: T): Unit = {
      elems.foreach { in =>
        in.changed -/-> changed
      }
    }

    def changed: IChangeEvent[T, Seq[A]] = this

    private[lucre] def pullChange(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): Seq[A] = {
      val b = Seq.newBuilder[A]
      b.sizeHint(elems)
      elems.foreach { in =>
        val v = pull.expr(in)
        b += v
      }
      b.result()
    }
  }

  private final class CountExpanded[T <: Txn[T], A](in: IExpr[T, Seq[A]], it: It.Expanded[T, A],
                                                    fun: Ex[Boolean], tx0: T)
                                                   (implicit targets: ITargets[T], ctx: Context[T])
    extends ExpandedMapSeqIn[T, A, Boolean, Int](in, it, fun, tx0) {

    override def toString: String = s"$in.count($fun)"

    protected def emptyOut: Int = 0

    protected def buildResult(inV: Seq[A], tuples: Tuples)(elem: IExpr[T, Boolean] => Boolean)
                             (implicit tx: T): Int = {
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

  object Count extends ProductReader[Count[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Count[_] = {
      require (arity == 3 && adj == 0)
      val _in = in.readEx[Seq[Any]]()
      val _it = in.readProductT[It[Any]]()
      val _p  = in.readEx[Boolean]()
      new Count(_in, _it, _p)
    }
  }
  final case class Count[A](in: Ex[Seq[A]], it: It[A], p: Ex[Boolean])
    extends Ex[Int] {

    type Repr[T <: Txn[T]] = IExpr[T, Int]

    override def productPrefix: String = s"ExSeq$$Count" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val inEx = in.expand[T]
      val itEx = it.expand[T]
      import ctx.targets
      new CountExpanded[T, A](inEx, itEx, p, tx)
    }
  }

  private final class DropWhileExpanded[T <: Txn[T], A](in: IExpr[T, Seq[A]], it: It.Expanded[T, A],
                                                        fun: Ex[Boolean], tx0: T)
                                                       (implicit targets: ITargets[T], ctx: Context[T])
    extends ExpandedMapSeqIn[T, A, Boolean, Seq[A]](in, it, fun, tx0) {

    override def toString: String = s"$in.dropWhile($fun)"

    protected def emptyOut: Seq[A] = Nil

    protected def buildResult(inV: Seq[A], tuples: Tuples)(elem: IExpr[T, Boolean] => Boolean)
                             (implicit tx: T): Seq[A] = {
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

  object DropWhile extends ProductReader[DropWhile[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): DropWhile[_] = {
      require (arity == 3 && adj == 0)
      val _in = in.readEx[Seq[Any]]()
      val _it = in.readProductT[It[Any]]()
      val _p  = in.readEx[Boolean]()
      new DropWhile(_in, _it, _p)
    }
  }
  final case class DropWhile[A](in: Ex[Seq[A]], it: It[A], p: Ex[Boolean])
    extends Ex[Seq[A]] {

    type Repr[T <: Txn[T]] = IExpr[T, Seq[A]]

    override def productPrefix: String = s"ExSeq$$DropWhile" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val inEx = in.expand[T]
      val itEx = it.expand[T]
      import ctx.targets
      new DropWhileExpanded[T, A](inEx, itEx, p, tx)
    }
  }

  private final class ExistsExpanded[T <: Txn[T], A](in: IExpr[T, Seq[A]], it: It.Expanded[T, A],
                                                     fun: Ex[Boolean], tx0: T)
                                                    (implicit targets: ITargets[T], ctx: Context[T])
    extends ExpandedMapSeqIn[T, A, Boolean, Boolean](in, it, fun, tx0) {

    override def toString: String = s"$in.exists($fun)"

    protected def emptyOut: Boolean = false

    protected def buildResult(inV: Seq[A], tuples: Tuples)(elem: IExpr[T, Boolean] => Boolean)
                             (implicit tx: T): Boolean = {
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

  object Exists extends ProductReader[Exists[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Exists[_] = {
      require (arity == 3 && adj == 0)
      val _in = in.readEx[Seq[Any]]()
      val _it = in.readProductT[It[Any]]()
      val _p  = in.readEx[Boolean]()
      new Exists(_in, _it, _p)
    }
  }
  final case class Exists[A](in: Ex[Seq[A]], it: It[A], p: Ex[Boolean])
    extends Ex[Boolean] {

    type Repr[T <: Txn[T]] = IExpr[T, Boolean]

    override def productPrefix: String = s"ExSeq$$Exists" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val inEx = in.expand[T]
      val itEx = it.expand[T]
      import ctx.targets
      new ExistsExpanded[T, A](inEx, itEx, p, tx)
    }
  }

  private final class FilterExpanded[T <: Txn[T], A](in: IExpr[T, Seq[A]], it: It.Expanded[T, A],
                                                     fun: Ex[Boolean], tx0: T)
                                                    (implicit targets: ITargets[T], ctx: Context[T])
    extends ExpandedMapSeqIn[T, A, Boolean, Seq[A]](in, it, fun, tx0) {

    override def toString: String = s"$in.filter($fun)"

    protected def emptyOut: Seq[A] = Nil

    protected def buildResult(inV: Seq[A], tuples: Tuples)(elem: IExpr[T, Boolean] => Boolean)
                             (implicit tx: T): Seq[A] = {
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

  object Filter extends ProductReader[Filter[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Filter[_] = {
      require (arity == 3 && adj == 0)
      val _in = in.readEx[Seq[Any]]()
      val _it = in.readProductT[It[Any]]()
      val _p  = in.readEx[Boolean]()
      new Filter(_in, _it, _p)
    }
  }
  final case class Filter[A](in: Ex[Seq[A]], it: It[A], p: Ex[Boolean])
    extends Ex[Seq[A]] {

    type Repr[T <: Txn[T]] = IExpr[T, Seq[A]]

    override def productPrefix: String = s"ExSeq$$Filter" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val inEx = in.expand[T]
      val itEx = it.expand[T]
      import ctx.targets
      new FilterExpanded[T, A](inEx, itEx, p, tx)
    }
  }

  private final class FilterNotExpanded[T <: Txn[T], A](in: IExpr[T, Seq[A]], it: It.Expanded[T, A],
                                                        fun: Ex[Boolean], tx0: T)
                                                       (implicit targets: ITargets[T], ctx: Context[T])
    extends ExpandedMapSeqIn[T, A, Boolean, Seq[A]](in, it, fun, tx0) {

    override def toString: String = s"$in.filterNot($fun)"

    protected def emptyOut: Seq[A] = Nil

    protected def buildResult(inV: Seq[A], tuples: Tuples)(elem: IExpr[T, Boolean] => Boolean)
                             (implicit tx: T): Seq[A] = {
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

  object FilterNot extends ProductReader[FilterNot[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): FilterNot[_] = {
      require (arity == 3 && adj == 0)
      val _in = in.readEx[Seq[Any]]()
      val _it = in.readProductT[It[Any]]()
      val _p  = in.readEx[Boolean]()
      new FilterNot(_in, _it, _p)
    }
  }
  final case class FilterNot[A](in: Ex[Seq[A]], it: It[A], p: Ex[Boolean])
    extends Ex[Seq[A]] {

    type Repr[T <: Txn[T]] = IExpr[T, Seq[A]]

    override def productPrefix: String = s"ExSeq$$FilterNot" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val inEx = in.expand[T]
      val itEx = it.expand[T]
      import ctx.targets
      new FilterNotExpanded[T, A](inEx, itEx, p, tx)
    }
  }

  private final class ForallExpanded[T <: Txn[T], A](in: IExpr[T, Seq[A]], it: It.Expanded[T, A],
                                                     fun: Ex[Boolean], tx0: T)
                                                    (implicit targets: ITargets[T], ctx: Context[T])
    extends ExpandedMapSeqIn[T, A, Boolean, Boolean](in, it, fun, tx0) {

    override def toString: String = s"$in.forall($fun)"

    protected def emptyOut: Boolean = true

    protected def buildResult(inV: Seq[A], tuples: Tuples)(elem: IExpr[T, Boolean] => Boolean)
                             (implicit tx: T): Boolean = {
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


  object Forall extends ProductReader[Forall[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Forall[_] = {
      require (arity == 3 && adj == 0)
      val _in = in.readEx[Seq[Any]]()
      val _it = in.readProductT[It[Any]]()
      val _p  = in.readEx[Boolean]()
      new Forall(_in, _it, _p)
    }
  }
  final case class Forall[A](in: Ex[Seq[A]], it: It[A], p: Ex[Boolean])
    extends Ex[Boolean] {

    type Repr[T <: Txn[T]] = IExpr[T, Boolean]

    override def productPrefix: String = s"ExSeq$$Forall" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val inEx = in.expand[T]
      val itEx = it.expand[T]
      import ctx.targets
      new ForallExpanded[T, A](inEx, itEx, p, tx)
    }
  }

  private final class FindExpanded[T <: Txn[T], A](in: IExpr[T, Seq[A]], it: It.Expanded[T, A],
                                                   fun: Ex[Boolean], tx0: T)
                                                  (implicit targets: ITargets[T], ctx: Context[T])
    extends ExpandedMapSeqIn[T, A, Boolean, Option[A]](in, it, fun, tx0) {

    override def toString: String = s"$in.find($fun)"

    protected def emptyOut: Option[A] = None

    protected def buildResult(inV: Seq[A], tuples: Tuples)(elem: IExpr[T, Boolean] => Boolean)
                             (implicit tx: T): Option[A] = {
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

  object Find extends ProductReader[Find[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Find[_] = {
      require (arity == 3 && adj == 0)
      val _in = in.readEx[Seq[Any]]()
      val _it = in.readProductT[It[Any]]()
      val _p  = in.readEx[Boolean]()
      new Find(_in, _it, _p)
    }
  }
  final case class Find[A](in: Ex[Seq[A]], it: It[A], p: Ex[Boolean])
    extends Ex[Option[A]] {

    type Repr[T <: Txn[T]] = IExpr[T, Option[A]]

    override def productPrefix: String = s"ExSeq$$Find" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val inEx = in.expand[T]
      val itEx = it.expand[T]
      import ctx.targets
      new FindExpanded[T, A](inEx, itEx, p, tx)
    }
  }

  private final class FindLastExpanded[T <: Txn[T], A](in: IExpr[T, Seq[A]], it: It.Expanded[T, A],
                                                       fun: Ex[Boolean], tx0: T)
                                                      (implicit targets: ITargets[T], ctx: Context[T])
    extends ExpandedMapSeqIn[T, A, Boolean, Option[A]](in, it, fun, tx0) {

    override def toString: String = s"$in.findLast($fun)"

    protected def emptyOut: Option[A] = None

    protected def buildResult(inV: Seq[A], tuples: Tuples)(elem: IExpr[T, Boolean] => Boolean)
                             (implicit tx: T): Option[A] = {
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

  object FindLast extends ProductReader[FindLast[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): FindLast[_] = {
      require (arity == 3 && adj == 0)
      val _in = in.readEx[Seq[Any]]()
      val _it = in.readProductT[It[Any]]()
      val _p  = in.readEx[Boolean]()
      new FindLast(_in, _it, _p)
    }
  }
  final case class FindLast[A](in: Ex[Seq[A]], it: It[A], p: Ex[Boolean])
    extends Ex[Option[A]] {

    type Repr[T <: Txn[T]] = IExpr[T, Option[A]]

    override def productPrefix: String = s"ExSeq$$FindLast" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val inEx = in.expand[T]
      val itEx = it.expand[T]
      import ctx.targets
      new FindLastExpanded[T, A](inEx, itEx, p, tx)
    }
  }

  private final class IndexWhereExpanded[T <: Txn[T], A](in: IExpr[T, Seq[A]], it: It.Expanded[T, A],
                                                         fun: Ex[Boolean], tx0: T)
                                                        (implicit targets: ITargets[T], ctx: Context[T])
    extends ExpandedMapSeqIn[T, A, Boolean, Int](in, it, fun, tx0) {

    override def toString: String = s"$in.indexWhere($fun)"

    protected def emptyOut: Int = -1

    protected def buildResult(inV: Seq[A], tuples: Tuples)(elem: IExpr[T, Boolean] => Boolean)
                             (implicit tx: T): Int = {
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

  object IndexWhere extends ProductReader[IndexWhere[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): IndexWhere[_] = {
      require (arity == 3 && adj == 0)
      val _in = in.readEx[Seq[Any]]()
      val _it = in.readProductT[It[Any]]()
      val _p  = in.readEx[Boolean]()
      new IndexWhere(_in, _it, _p)
    }
  }
  final case class IndexWhere[A](in: Ex[Seq[A]], it: It[A], p: Ex[Boolean])
    extends Ex[Int] {

    type Repr[T <: Txn[T]] = IExpr[T, Int]

    override def productPrefix: String = s"ExSeq$$IndexWhere" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val inEx = in.expand[T]
      val itEx = it.expand[T]
      import ctx.targets
      new IndexWhereExpanded[T, A](inEx, itEx, p, tx)
    }
  }

  // XXX TODO --- we should use cell-views instead, because this way we won't notice
  // changes to the value representation (e.g. a `StringObj.Var` contents change)
  private final class SelectExpanded[T <: Txn[T], A](in: IExpr[T, Seq[Obj]], tx0: T)
                                                    (implicit targets: ITargets[T], bridge: Obj.Bridge[A])
    extends MappedIExpr[T, Seq[Obj], Seq[A]](in, tx0) {

    protected def mapValue(inValue: Seq[Obj])(implicit tx: T): Seq[A] =
      inValue.flatMap(_.peer[T].flatMap(bridge.tryParseObj(_)))
  }

  object Select extends ProductReader[Select[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Select[_] = {
      require (arity == 1 && adj == 1)
      val _in = in.readEx[Seq[Obj]]()
      val _bridge: Obj.Bridge[Any] = in.readAdjunct()
      new Select[Any](_in)(_bridge)
    }
  }
  final case class Select[A](in: Ex[Seq[Obj]])(implicit bridge: Obj.Bridge[A])
    extends Ex[Seq[A]] with ProductWithAdjuncts {

    type Repr[T <: Txn[T]] = IExpr[T, Seq[A]]

    override def productPrefix: String = s"ExSeq$$Select" // serialization

    def adjuncts: List[Adjunct] = bridge :: Nil

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val inEx = in.expand[T]
      import ctx.targets
      new SelectExpanded[T, A](inEx, tx)
    }
  }

  // XXX TODO --- we should use cell-views instead, because this way we won't notice
  // changes to the value representation (e.g. a `StringObj.Var` contents change)
  private final class SelectFirstExpanded[T <: Txn[T], A](in: IExpr[T, Seq[Obj]], tx0: T)
                                                         (implicit targets: ITargets[T], bridge: Obj.Bridge[A])
    extends MappedIExpr[T, Seq[Obj], Option[A]](in, tx0) {

    protected def mapValue(inValue: Seq[Obj])(implicit tx: T): Option[A] = {
      val it = inValue.iterator.flatMap(_.peer[T].flatMap(bridge.tryParseObj(_)))
      if (it.hasNext) Some(it.next()) else None
    }
  }

  object SelectFirst extends ProductReader[SelectFirst[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): SelectFirst[_] = {
      require (arity == 1 && adj == 1)
      val _in = in.readEx[Seq[Obj]]()
      val _bridge: Obj.Bridge[Any] = in.readAdjunct()
      new SelectFirst[Any](_in)(_bridge)
    }
  }
  final case class SelectFirst[A](in: Ex[Seq[Obj]])(implicit bridge: Obj.Bridge[A])
    extends Ex[Option[A]] with ProductWithAdjuncts {

    type Repr[T <: Txn[T]] = IExpr[T, Option[A]]

    override def productPrefix: String = s"ExSeq$$SelectFirst" // serialization

    def adjuncts: List[Adjunct] = bridge :: Nil

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val inEx = in.expand[T]
      import ctx.targets
      new SelectFirstExpanded[T, A](inEx, tx)
    }
  }

  private final class TakeWhileExpanded[T <: Txn[T], A](in: IExpr[T, Seq[A]], it: It.Expanded[T, A],
                                                        fun: Ex[Boolean], tx0: T)
                                                       (implicit targets: ITargets[T], ctx: Context[T])
    extends ExpandedMapSeqIn[T, A, Boolean, Seq[A]](in, it, fun, tx0) {

    override def toString: String = s"$in.takeWhile($fun)"

    protected def emptyOut: Seq[A] = Nil

    protected def buildResult(inV: Seq[A], tuples: Tuples)(elem: IExpr[T, Boolean] => Boolean)
                             (implicit tx: T): Seq[A] = {
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

  object TakeWhile extends ProductReader[TakeWhile[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): TakeWhile[_] = {
      require (arity == 3 && adj == 0)
      val _in = in.readEx[Seq[Any]]()
      val _it = in.readProductT[It[Any]]()
      val _p  = in.readEx[Boolean]()
      new TakeWhile(_in, _it, _p)
    }
  }
  final case class TakeWhile[A](in: Ex[Seq[A]], it: It[A], p: Ex[Boolean])
    extends Ex[Seq[A]] {

    type Repr[T <: Txn[T]] = IExpr[T, Seq[A]]

    override def productPrefix: String = s"ExSeq$$TakeWhile" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val inEx = in.expand[T]
      val itEx = it.expand[T]
      import ctx.targets
      new TakeWhileExpanded[T, A](inEx, itEx, p, tx)
    }
  }

  override def read(in: RefMapIn, key: String, arity: Int, adj: Int): ExSeq[_] = {
    require (arity == 1 && adj == 0)
    val _elems = in.readVec(in.readEx[Any]())
    new ExSeq(_elems: _*)
  }
}
final case class ExSeq[A](elems: Ex[A]*) extends Ex[Seq[A]] {

  type Repr[T <: Txn[T]] = IExpr[T, Seq[A]]

  private def simpleString: String = {
    val xs = elems.iterator.take(5).toList
    val es = if (xs.lengthCompare(5) == 0) xs.init.mkString("", ", ", ", ...")
    else xs.mkString(", ")
    s"ExSeq($es)"
  }

  override def toString: String = simpleString

  protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
    import ctx.targets
    val elemsEx: Seq[IExpr[T, A]] = elems.iterator.map(_.expand[T]).toList
    new expr.ExSeq.Expanded(elemsEx).init()
  }
}
