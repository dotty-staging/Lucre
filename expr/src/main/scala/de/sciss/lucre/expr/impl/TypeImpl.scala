/*
 *  TypeImpl.scala
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

package de.sciss.lucre.expr.impl

import de.sciss.lucre.event.Targets
import de.sciss.lucre.expr.Type
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.serial.DataInput

import scala.language.higherKinds

trait TypeImplLike[Ext >: Null <: Type.Extension] extends Obj.Type {
  // implicit protected def extTag: reflect.ClassTag[Ext]

  protected def mkExtArray(size: Int): Array[Ext]

  final protected def addExtension(extensions: Array[Ext], ext: Ext): Array[Ext] = {
    val opLo = ext.opLo
    val opHi = ext.opHi
    require (opLo <= opHi, s"Lo ($opLo) must be less than or equal hi ($opHi)")
    val idx0  = extensions.indexWhere(_.opLo > opHi)
    val idx   = if (idx0 < 0) extensions.length else idx0
    if (idx > 0) {
      val pred = extensions(idx - 1)
      require(pred.opHi < opLo, s"Extension overlap for $pred versus $ext")
    }
    val len   = extensions.length
    val extensions1 = mkExtArray(len + 1) // new Array[Ext](len + 1)
    System.arraycopy(extensions, 0, extensions1, 0, len)
    extensions1(len) = ext
    extensions1
  }

  final protected def findExt(extensions: Array[Ext], op: Int): Ext = {
    var index = 0
    var low   = 0
    var high  = extensions.length - 1
    while ({
      index = (high + low) >> 1
      low  <= high
    }) {
      val ext = extensions(index)
      if (ext.opLo <= op) {
        if (ext.opHi >= op) return ext
        low = index + 1
      } else {
        high = index - 1
      }
    }
    null
  }
}
trait TypeImpl[Ext >: Null <: Type.Extension] extends TypeImplLike[Ext] {
  private[this] var extensions = mkExtArray(0) // new Array[Ext](0)

  final def registerExtension(ext: Ext): Unit = extensions = addExtension(extensions, ext)

  final protected def findExt(op: Int): Ext = findExt(extensions, op)
}

trait TypeImpl1[Repr[~ <: Sys[~]]] extends TypeImpl[Type.Extension1[Repr]] with Type._1[Repr] {
  // final protected val extTag = reflect.classTag[Type.Extension1[Repr]]

  protected def mkExtArray(size: Int): Array[Type.Extension1[Repr]] = new Array(size)

  final protected def readExtension[S <: Sys[S]](op: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                                                (implicit tx: S#Tx): Repr[S] = {
    val ext = findExt(op)
    if (ext == null) sys.error(s"Unknown extension operator $op")
    ext.readExtension[S](op, in, access, targets)
  }
}