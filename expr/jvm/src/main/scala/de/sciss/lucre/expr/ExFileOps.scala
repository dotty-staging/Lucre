package de.sciss.lucre.expr

import de.sciss.lucre.expr.graph.{Act, Ex, File, UnaryOp => UnOp, BinaryOp => BinOp}
import de.sciss.file.{File => _File}

final class ExFileOps(private val x: Ex[_File]) extends AnyVal {
  // ---- expressions ----

  /** Returns the parent directory if it exists. */
  def parentOption: Ex[Option[_File]] =
    UnOp(UnOp.FileParentOption(), x)

  /** Returns the string representation of the file's path. */
  def path: Ex[String] =
    UnOp(UnOp.FilePath(), x)

  /** Returns the name part of the file. */
  def name: Ex[String] =
    UnOp(UnOp.FileName(), x)

  /** Returns the name part of the file and drops the extension (if any). */
  def base: Ex[String] =
    UnOp(UnOp.FileBase(), x)

  /** Returns the extension of the file (lower-cased, period dropped). Returns and empty string
   * if no extension is given.
   */
  def ext: Ex[String] =
    UnOp(UnOp.FileExtL(), x)  // ! simplify and use lower case here

  /** Replaces the extension part of this file. Parameter `s` may or may not contain a leading period. */
  def replaceExt(s: Ex[String]): Ex[_File] =
    BinOp(BinOp.FileReplaceExt(), x, s)

  /** Replaces the name part of this file, keeping the parent directory. */
  def replaceName(s: Ex[String]): Ex[_File] =
    BinOp(BinOp.FileReplaceName(), x, s)

  def / (child: Ex[String]): Ex[_File] =
    BinOp(BinOp.FileChild(), x, child)

  // ---- actions ----

  /** Deletes the file */
  def delete: Act = File.Delete(x)

  /** Creates the directory and possibly parent directories denoted by this file. */
  def mkDir : Act = File.MkDir(x)

  // ---- hybrid ----

  /** Lists the contains of a directory */
  def list: Ex[Seq[_File]] with Act = File.List(x)
}
