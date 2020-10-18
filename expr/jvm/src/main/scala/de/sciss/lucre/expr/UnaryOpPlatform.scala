package de.sciss.lucre.expr

import de.sciss.file._
import de.sciss.lucre.expr.graph.UnaryOp.NamedOp

trait UnaryOpPlatform {
  // ---- File ----

  final case class FileParentOption() extends NamedOp[File, Option[File]] {
    def apply(a: File): Option[File] = a.parentOption

    def name = "FileParentOption"
  }

  final case class FilePath() extends NamedOp[File, String] {
    def apply(a: File): String = a.path

    def name = "FilePath"
  }

  final case class FileName() extends NamedOp[File, String] {
    def apply(a: File): String = a.name

    def name = "FileName"
  }

  final case class FileBase() extends NamedOp[File, String] {
    def apply(a: File): String = a.base

    def name = "FileBase"
  }

  final case class FileExtL() extends NamedOp[File, String] {
    def apply(a: File): String = a.extL

    def name = "FileExtL"
  }
}
