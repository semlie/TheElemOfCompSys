
package ex7.test1
import ex8.cs.translator

import ex8.cs.helpers

object run {
  def main(args: Array[String]) {
    if (args.length > 0) {
      // val   path = "C:/tmp/p7/StackArithmetic/StackTest/"
      val Fullpath = args(0)//"C:/tmp/p7/MemoryAccess/StaticTest/"
      val path = Fullpath.split("/").last
      // val  fileName = "StackTest.vm"
      val fileName = "StaticTest.vm"
      val files = helpers.fileList(Fullpath)

      val trans = new translator

      //
      for (x <- files) {

        trans.transAll(Fullpath +path, helpers.allFileLines(x), trans.pars,x)
      }
      println("I did IT")
    } else {
      println("NO PATH ARGOMENT")
    }
  }
}