package ex7.test1
import java.io._
object HelloWorld extends App {
  def using[A <: { def close(): Unit }, B](param: A)(f: A => B): B =
    try { f(param) } finally { param.close() }

  def appendToFile(file: File, textData: String) =
    using(new FileWriter(file, true)) {
      fileWriter =>
        using(new PrintWriter(fileWriter)) {
          printWriter => printWriter.println(textData)
        }
    }

  val path = "c://tmp/"
  var counter = 0;
  def increcer = { counter += 1; counter }
  val fileList = new java.io.File(path).listFiles.filter(_.getName.endsWith(".in"))

  //def fileLines(file: java.io.File) = io.Source.fromFile(file).getLines.toList

  //fileList.foreach(fileLines(_).foreach(println))
  val outFile = new File(path + "hello.out")
  val allFileLines = io.Source.fromFile("c://tmp/hello.in").getLines
  allFileLines.foreach(appendToFile(outFile, _))

  val filterLines = io.Source.fromFile("c://tmp/hello.in").getLines.filter(_.contains("you"))
  filterLines.foreach(println)
  //  fileList.foreach(appendToFile(_,increcer.toString))

}
