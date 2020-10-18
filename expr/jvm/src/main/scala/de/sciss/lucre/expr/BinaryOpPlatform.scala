package de.sciss.lucre.expr

import de.sciss.file._
import de.sciss.lucre.expr.graph.BinaryOp.NamedOp

trait BinaryOpPlatform {
  // ---- File ----

  final case class FileReplaceExt() extends NamedOp[File, String, File] {
    def apply(a: File, s: String): File = a.replaceExt(s)

    def name = "FileReplaceExt"
  }

  final case class FileReplaceName() extends NamedOp[File, String, File] {
    def apply(a: File, s: String): File = a.replaceName(s)

    def name: String = "FileReplaceName"
  }

  final case class FileChild() extends NamedOp[File, String, File] {
    def apply(a: File, s: String): File = a./(s)

    def name: String = "FileChild"
  }
}
