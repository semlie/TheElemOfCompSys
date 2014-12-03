
package ex7.test1
import ex7.cs.translator

import ex7.cs.helpers

object run {
  def main(args: Array[String]) {

   // val   path = "C:/tmp/p7/StackArithmetic/StackTest/"
	 val   path =  "C:/tmp/p7/MemoryAccess/StaticTest/"
   // val  fileName = "StackTest.vm"
     val  fileName = "StaticTest.vm"
    val files = helpers.fileList(path)
    val a = helpers.allFileLines(path.concat(fileName))
    val b = new translator
    b.transAll(path.concat(fileName), a, b.pars)
    println("I did IT")
  }

}
