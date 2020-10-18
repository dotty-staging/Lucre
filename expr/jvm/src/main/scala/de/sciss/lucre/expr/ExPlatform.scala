package de.sciss.lucre.expr

import de.sciss.lucre.Adjunct
import de.sciss.lucre.Adjunct.{FromAny, HasDefault, Ord, Scalar, ScalarOrd}
import de.sciss.serial.DataInput
import de.sciss.file.{File => _File}
import de.sciss.equal.Implicits._
import de.sciss.lucre.expr.graph.Ex

trait ExPlatform {
  protected lazy val _initPlatform: Unit = {
    Adjunct.addFactory(FileTop)
  }

  implicit def fileOps(x: Ex[_File]): ExFileOps = new ExFileOps(x)

  def fileTop: FromAny[_File] with HasDefault[_File] with ScalarOrd[_File] = FileTop

  private object FileTop extends FromAny[_File] with HasDefault[_File] with Ord[_File] with Scalar[_File]
    with Adjunct.Factory {

    final val id = 1015

    def readIdentifiedAdjunct(in: DataInput): Adjunct = this

    def defaultValue: _File = new _File("") // by convention

    def fromAny(in: Any): Option[_File] = in match {
      case v: _File     => Some(v)
      case _            => None
    }

    def lt  (a: _File, b: _File): Boolean = _File.NameOrdering.lt(a, b)
    def lteq(a: _File, b: _File): Boolean = _File.NameOrdering.lteq(a, b)
    def gt  (a: _File, b: _File): Boolean = _File.NameOrdering.gt(a, b)
    def gteq(a: _File, b: _File): Boolean = _File.NameOrdering.gteq(a, b)
    def eq  (a: _File, b: _File): Boolean = _File.NameOrdering.compare(a, b) === 0
    def neq (a: _File, b: _File): Boolean = _File.NameOrdering.compare(a, b) !== 0
  }

}
