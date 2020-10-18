package de.sciss.lucre.expr

import de.sciss.lucre.Adjunct.{FromAny, HasDefault, ScalarOrd}
import de.sciss.lucre.expr.graph.Ex
import de.sciss.file.{File => _File}
import de.sciss.lucre.expr.graph.Ex.Value

trait ExImportPlatform {
  implicit def fileTop: FromAny[_File] with HasDefault[_File] with ScalarOrd[_File] = Ex.fileTop

  implicit object fileIsValue extends Value[_File]
}
