package ex10.cs
import scala.util.parsing.combinator._;
import scala.annotation.tailrec

import scala.io._
import scala.swing._
import java.io._
import scala.annotation.tailrec
import scala.annotation.tailrec
import scala.swing.FileChooser.Result.Approve
import scala.swing.FileChooser.Result.Cancel
import scala.swing.FileChooser.Result.Error

object test4 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
import scala.util.parsing.combinator._

// wholeNumber and stringLiteral are provided by JavaTokenParsers
object RepeatParser extends JavaTokenParsers {
  var count = 0

  def repeat = "repeat" ~> n <~ "times" ~ block
  def n      = wholeNumber ^^ { reps => count = reps.toInt }
  def block  = "{" ~> lines <~ "}"
  def lines  = rep(line)
  def line   = "say" ~> message ^^ { msg =>
    for (i <- 1 to count) println(msg)
  }
  def message = stringLiteral
}

val input = """repeat 10 times {
  say "hello"
  say "world!"
}"""                                              //> input  : String = repeat 10 times {
                                                  //|   say "hello"
                                                  //|   say "world!"
                                                  //| }

RepeatParser.parseAll(RepeatParser.repeat, input) //> "hello"
                                                  //| "hello"
                                                  //| "hello"
                                                  //| "hello"
                                                  //| "hello"
                                                  //| "hello"
                                                  //| "hello"
                                                  //| "hello"
                                                  //| "hello"
                                                  //| "hello"
                                                  //| "world!"
                                                  //| "world!"
                                                  //| "world!"
                                                  //| "world!"
                                                  //| "world!"
                                                  //| "world!"
                                                  //| "world!"
                                                  //| "world!"
                                                  //| "world!"
                                                  //| "world!"
                                                  //| res0: ex10.cs.test4.RepeatParser.ParseResult[Unit] = [4.2] parsed: ()


object RepeatParser2 extends JavaTokenParsers {
  var count = 0
  var messages = new scala.collection.mutable.ArrayBuffer[String]()
  
  def repeat = ("repeat" ~> n <~ "times" ~ block)
  def n      = stringLiteral ^^ { s =>
    println("idiot!! Bad invocation: "+s)
    } | wholeNumber ^^ { reps => count = reps.toInt }
  def block  = "{" ~> lines <~ "}" ^^ { _ =>
    for (i <- 1 to count) messages foreach println
  }
  def lines  = rep(line)
  def line   = "say" ~> message ^^ { msg =>
    messages + msg
  }
  def message = stringLiteral
}

val input2 = """repeat 10 times {
  say "hello ..."
  say "... world!"
}"""                                              //> input2  : String = repeat 10 times {
                                                  //|   say "hello ..."
                                                  //|   say "... world!"
                                                  //| }
 RepeatParser2.parseAll(RepeatParser2.repeat, input2)
                                                  //> res1: ex10.cs.test4.RepeatParser2.ParseResult[Unit] = [4.2] parsed: ()
}