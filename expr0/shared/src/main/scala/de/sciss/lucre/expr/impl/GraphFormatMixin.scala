/*
 *  GraphFormatMixin.scala
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

package de.sciss.lucre.expr.impl

import de.sciss.lucre.expr.ExElem
import de.sciss.lucre.expr.graph.Control

import scala.collection.immutable.{IndexedSeq => Vec}

trait GraphFormatMixin {
  protected final def writeControls(controls: Vec[Control.Configured],
                                    ref: ExElem.RefMapOut): Unit = {
    val out = ref.out
    out.writeInt(controls.size)
    controls.foreach { conf =>
      ref.writeProduct(conf.control) // ExElem.write(conf.control, out, ref)
      val m = conf.properties
      out.writeInt(m.size)
      m.foreach { case (key, v) =>
        out.writeUTF(key)
        ref.writeElem(v)
      }
    }
  }

  protected final def readControls(ref: ExElem.RefMapIn): Vec[Control.Configured] = {
    val in  = ref.in
    val szC = in.readInt()
    val cxb = Vec.newBuilder[Control.Configured]
    cxb.sizeHint(szC)
    var i = 0
    while (i < szC) {
      val ctl = ref.readProductT[Control]()
      // not ref.readMap because we don't (unfortunately?) encode the 'S' key type
      val mSz = in.readInt()
      val mb  = Map.newBuilder[String, Any]
      mb.sizeHint(mSz)
      var j = 0
      while (j < mSz) {
        val k = in.readUTF()
        val v = ref.readElem()
        mb += k -> v
        j  += 1
      }
      val properties = mb.result()
      val configured = Control.Configured(ctl, properties)
      cxb += configured
      i   += 1
    }
    cxb.result()
  }
}

