package de.sciss.lucre.expr

import de.sciss.lucre.Event.Targets
import de.sciss.lucre.Log.{event => logEvent}
import de.sciss.lucre.edit.UndoManager
import de.sciss.lucre.expr.graph.{Ex, Folder}
import de.sciss.lucre.expr.impl.ContextMixin
import de.sciss.lucre.impl.{ExprNodeImpl, ExprTypeExtension1, GeneratorEvent}
import de.sciss.lucre.store.BerkeleyDB
import de.sciss.lucre.{BooleanObj, Caching, Copy, Cursor, Disposable, Durable, Event, EventLike, IntObj, Pull, Source, Txn, Workspace, Folder => LFolder, Obj => LObj, Var => LVar}
import de.sciss.model.Change
import de.sciss.serial.{DataInput, DataOutput}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.collection.mutable

object IntExObjTest {
  type S = Durable
  type T = Durable.Txn

  //  type S = InMemory
  //  type T = InMemory.Txn

  def main(args: Array[String]): Unit = {
    LucreExpr   .init()
//    LFolder       .init()
//    Ex            .init()
//    graph.Obj     .init()
    IntExObj      .init()

    //    Log.event.level = Level.Debug

    //    implicit val system: S = InMemory()
    val store = BerkeleyDB.tmp()
    implicit val system: S = Durable(store)

    val (inH, outH) = system.step { implicit tx =>
      import ExImport._
      //      val ex: Ex[Int] = "in".attr(0) * 2
      val ex: Ex[Int] = "in".attr(Folder()).size
      //      val input     = IntObj.newVar[T](0)
      val input     = LFolder[T]()
      val transform = IntExObj[T](ex)
      println("--- put 'in'")
      transform.attr.put("in", input)
      val output    = IntObj.newVar[T](transform)
      (tx.newHandle(input), tx.newHandle(output))
    }

    system.step { implicit tx =>
      val out = outH()
      println("--- add react")
      out.changed.react { implicit tx => upd =>
        println(s"OBSERVED: $upd")
      }
    }

    system.step { implicit tx =>
      val in = inH()
      println("--- update 'in'")
      //      in() = 1000
      in.addLast(BooleanObj.newConst(false))
    }

    val v = system.step { implicit tx =>
      val out = outH()
      println("--- call 'value'")
      out.value
    }
    println(s"OUTPUT now $v")

    system.close()
  }
}

object IntExObj {
  private lazy val _init: Unit = IntObj.registerExtension(IntEx)

  def init(): Unit = _init

  def apply[T <: Txn[T]](ex: Ex[Int])(implicit tx: T): IntObj[T] = {
    val targets = Targets[T]()
    // println("--> begin new")
    val res = new IntExNew[T](ex, targets, tx)
    // println("<-- end new")
    res
  }

  private[this] final val INT_EX_SER_VERSION = 0x4945

  // XXX TODO is this viable?
  private[this] final class HeadlessContext[T <: Txn[T]](_selfH: Source[T, LObj[T]])
    extends ContextMixin[T] {

//    override def connect: Boolean = false

    private[this] val _eventsSet = mutable.Set.empty[Event[T, Any]]
    private[this] var _eventsVec = Vector     .empty[Event[T, Any]]

    def events: Vec[Event[T, Any]] = _eventsVec

    override def reactTo[A](event: EventLike[T, A])(fun: T => A => Unit)(implicit tx: T): Disposable[T] = {
      event match {
        case e: Event[T, A] =>
          val isNew = _eventsSet.add(e)
          logEvent.debug(s"ExObj register $e")
          if (isNew) _eventsVec :+= e
        case _ => ()
      }
      Disposable.empty
    }

    override protected def selfH: Option[Source[T, LObj[T]]] = Some(_selfH)

    private def unsupported(what: String): Nothing =
      throw new UnsupportedOperationException(s"$what of a headless context")

    override implicit def cursor      : Cursor      [T] = unsupported("cursor"      )
    override implicit def workspace   : Workspace   [T] = unsupported("workspace"   )
    override implicit def undoManager : UndoManager [T] = unsupported("undo-manager")

    override def attr: Context.Attr[T] = Context.emptyAttr
  }

  private[this] object IntEx extends ExprTypeExtension1[IntObj] {

    override def readExtension[T <: Txn[T]](opId: Int, in: DataInput, targets: Targets[T])
                                           (implicit tx: T): IntObj[T] = {
      // println("--> begin read")
      val res = new IntExRead[T](targets, in)
      // println("<-- end read")
      res
    }

    override def name: String = "Ex[Int]"

    override final val opLo = -1
    override final val opHi = -1
  }

  private final class IntExNew[T <: Txn[T]](protected val ex: Ex[Int], protected val targets: Targets[T], tx0: T)
    extends IntEx[T] {

    protected val sourcesRef: LVar[T, Vec[Event[T, Any]]] = {
      implicit val tx: T = tx0
      id.newVar(Vec.empty[Event[T, Any]])
    }

    protected val valueRef: LVar[T, A] = {
      implicit val tx: T = tx0
      id.newIntVar(0)
    }

    valueImpl(tx0)  // creates valueRef and sourceRef contents
  }

  private final class IntExRead[T <: Txn[T]](protected val targets: Targets[T], in0: DataInput)
    extends IntEx[T] {

    {
      val cookie = in0.readShort()
      require(cookie == INT_EX_SER_VERSION, s"Unexpected cookie $cookie")
    }

    protected val ex: Ex[Int] = {
      val ref   = new ExElem.RefMapIn(in0)
      ref.readEx[Int]()
    }

    protected val sourcesRef: LVar[T, Vec[Event[T, Any]]] = id.readVar[Vec[Event[T, Any]]](in0)

    protected val valueRef: LVar[T, A] = id.readIntVar(in0)
  }

  private abstract class IntEx[T <: Txn[T]] extends IntObj[T] with ExprNodeImpl[T, Int] {

    type A = Int

    override def toString = s"Expr$id @${hashCode.toHexString}"

    // ---- abstract ---

    protected def ex        : Ex[A]
    protected def sourcesRef: LVar[T, Vec[Event[T, Any]]]
    protected def valueRef  : LVar[T, A]

    // ---- impl ----

    override def value(implicit tx: T): Int = {
      val valueNew = valueImpl
      val valueOld = valueRef()
      if (valueNew != valueOld) valueRef() = valueNew
      valueNew
    }

    protected final def valueImpl(implicit tx: T): Int = {
      val hc = new HeadlessContext[T](tx.newHandle[IntObj[T]](this))
      implicit val ctx: Context[T] = hc
      val peer      = ex.expand[T]
      val res       = peer.value
      val eventsOld = sourcesRef()
      val eventsNew = hc.events
      if (eventsOld != eventsNew) {
        sourcesRef()  = eventsNew
        val eventsAdd = eventsNew diff eventsOld
        val eventsRem = eventsOld diff eventsNew
        // logEvent.debug(s"ExObj remove $eventsRem")
        // logEvent.debug(s"ExObj add    $eventsAdd")
        eventsRem.foreach(_ -/-> changed)
        eventsAdd.foreach(_ ---> changed)
      }
      res
    }

    override def tpe: LObj.Type = IntObj

    object changed extends Changed with GeneratorEvent[T, Change[A]] with Caching {
      private[lucre] def pullUpdate(pull: Pull[T])(implicit tx: T): Option[Change[A]] = {
        val valueOld  = valueRef()
        val valueNew  = value  // updates cache
        logEvent.debug(s"ExObj pullUpdate; $valueOld -> $valueNew")
        val ch        = Change(valueOld, valueNew)
        ch.toOption
      }
    }

    private def writeEx(out: DataOutput): Unit = {
      val ref = new ExElem.RefMapOut(out)
      ref.writeElem(ex)
    }

    override protected def writeData(out: DataOutput): Unit = {
      out.writeByte(1)  // 'node not var'
      out.writeInt(-1)  // opId
      out.writeShort(INT_EX_SER_VERSION)
      writeEx(out)
      sourcesRef.write(out)
      valueRef  .write(out)
    }

    override protected def disposeData()(implicit tx: T): Unit = {
      sourcesRef.swap(Vector.empty).foreach(_ -/-> changed)
    }

    /** Makes a deep copy of an element, possibly translating it to a different system `Out`. */
    override private[lucre] def copy[Out <: Txn[Out]]()(implicit txIn: T, txOut: Out,
                                                        context: Copy[T, Out]): IntObj[Out] = {
      val newTgt = Event.Targets[Out]()
      new IntExNew[Out](ex, newTgt, txOut)
    }
  }
}
