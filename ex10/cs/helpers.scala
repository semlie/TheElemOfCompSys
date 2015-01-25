package ex10.cs

import java.io._
import scala.util.matching.Regex
import scala.collection.mutable.Stack

object helpers {
  
  /**
   * help to write to files
   */
  def using[A <: { def close(): Unit }, B](param: A)(f: A => B): B =
    try { f(param) } finally { param.close() }

  def appendToFile(file: File, textData: String) =
    using(new FileWriter(file, true)) {
      fileWriter =>
        using(new PrintWriter(fileWriter)) {
          printWriter => printWriter.println(textData)
        }
    }

  
      def allFileLines(file: String) = io.Source.fromFile(file).getLines.filterNot(_.startsWith("//")).mkString


}