package de.sciss.lucre.expr.graph

import de.sciss.lucre.event.{IEvent, IPull, ITargets}
import de.sciss.lucre.event.impl.IEventImpl
import de.sciss.lucre.expr.{Ex, IExpr}
import de.sciss.lucre.stm.{Base, Sys}
import de.sciss.model.Change

import scala.collection.immutable.{Seq => ISeq}

object SeqMkString {
  private final class Expanded[S <: Base[S], A](in: IExpr[S, ISeq[A]], start: IExpr[S, String], sep: IExpr[S, String],
                                                stop: IExpr[S, String], tx0: S#Tx)
                                               (implicit protected val targets: ITargets[S])
    extends IExpr[S, String] with IEventImpl[S, Change[String]] {

    in    .changed.--->(this)(tx0)
    start .changed.--->(this)(tx0)
    sep   .changed.--->(this)(tx0)
    stop  .changed.--->(this)(tx0)

    override def toString: String = s"SeqMkString($in, $start, $sep, $stop)"

    def changed: IEvent[S, Change[String]] = this

    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Change[String]] = {
      val _inEvt    = in    .changed
      val _startEvt = start .changed
      val _sepEvt   = sep   .changed
      val _stopEvt  = stop  .changed

      val _inCh0    = if (pull.contains(_inEvt    )) pull(_inEvt    ) else None
      val _startCh0 = if (pull.contains(_startEvt )) pull(_startEvt ) else None
      val _sepCh0   = if (pull.contains(_sepEvt   )) pull(_sepEvt   ) else None
      val _stopCh0  = if (pull.contains(_stopEvt  )) pull(_stopEvt  ) else None
      
      
      val _inCh     = _inCh0    .getOrElse { val v = in    .value; Change(v, v) }
      val _startCh  = _startCh0 .getOrElse { val v = start .value; Change(v, v) }
      val _sepCh    = _sepCh0   .getOrElse { val v = sep   .value; Change(v, v) }
      val _stopCh   = _stopCh0  .getOrElse { val v = stop  .value; Change(v, v) }
      
      val before  = value1(_inCh.before , _startCh.before , _sepCh.before , _stopCh.before)
      val now     = value1(_inCh.now    , _startCh.now    , _sepCh.now    , _stopCh.now   )
      val ch      = Change(before, now)
      if (ch.isSignificant) Some(ch) else None
    }

    @inline
    private def value1(inV: ISeq[A], startV: String, sepV: String, stopV: String): String =
      inV.mkString(startV, sepV, stopV)

    def value(implicit tx: S#Tx): String = {
      val inV     = in    .value
      val startV  = start .value
      val sepV    = sep   .value
      val stopV   = stop  .value
      value1(inV, startV, sepV, stopV)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      in    .changed -/-> changed
      start .changed -/-> changed
      sep   .changed -/-> changed
      stop  .changed -/-> changed
    }
  }
}
final case class SeqMkString[A](in: Ex[ISeq[A]], start: Ex[String], sep: Ex[String], stop: Ex[String])
  extends Ex.Lazy[String] {

  protected def mkExpr[S <: Sys[S]](implicit ctx: Ex.Context[S], tx: S#Tx): IExpr[S, String] = {
    import ctx.targets
    val inExp     = in    .expand[S]
    val startExp  = start .expand[S]
    val sepExp    = sep   .expand[S]
    val stopExp   = stop  .expand[S]
    new SeqMkString.Expanded(in = inExp, start = startExp, sep = sepExp, stop = stopExp, tx0 = tx)
  }
}
