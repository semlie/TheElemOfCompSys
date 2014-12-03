package test1

import com.sun.org.apache.xalan.internal.xsltc.compiler.ForEach

object ex0 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  
  import java.nio.file.{Files, Path}
  val path = "c://tmp/"                               //
                                                  //> path  : String = c://tmp/
//val fileList = new java.io.File(path ).listFiles.filter(_.getName.endsWith(".in")).map(_.getName)

def fileLines(p:String)(x:String) = io.Source.fromFile(p+x).getLines.toList
                                                  //> fileLines: (p: String)(x: String)List[String]
    val b= fileLines(path)(_)                     //> b  : String => List[String] = <function1>
//for(as <- b)println(as)

//val a = fileList.foreach(fileLines(_))
// val a = fileList.foreach("c://tmp/hallo.in")
//println(fileLines("c://tmp/hello.in"))
}