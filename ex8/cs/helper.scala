package ex8.cs
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

  /**
   * 	get all files in dir that match the criteria endWith .vm
   */
  val fileList = (path: String) => new java.io.File(path).listFiles.filter(_.getName.endsWith(".vm")).map(path+"/"+_.getName)
 
  /**
   * 	append to file use it by given 1 time the file name and than in iteration only the string to add in
   */
  def writeTranslatToFile = (file: String, str: String) => appendToFile(new File(file.concat(".asm")), str)
  
  /**
   * 	get all file lines in list filtered if its comment
   */
  def allFileLines(file: String) = io.Source.fromFile(file).getLines.filterNot(_.startsWith("//")).toList.toArray

  
  def iterLines(l: String, func: String => Unit) = func(l)
  val reg = "^[^//]".r
  def filterCommand(s: String) = (reg findAllIn s)

}
