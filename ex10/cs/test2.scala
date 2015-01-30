package ex10.cs
import java.io.FileReader
object test2 {

  def main(args: Array[String]): Unit = {
    val pars = new parsing
      val reader = helpers.allFileLines(args(0))
//  val stream = """while(x > 0) {return x;}"""
//    val stream = """a + b"""
//    val stream = """printString("THE AVERAGE IS: ")"""
//    val stream = """printString(12)"""
//    val stream = """Output.printString("THE AVERAGE IS: ")"""
//    val stream = """sum + a[i];"""
    val stream = """let a = Array.new(length);"""
    var d = pars.parse(pars.letStatement, stream)
    
      println(d)
  }
  
  

}