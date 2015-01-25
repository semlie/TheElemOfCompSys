package ex10.cs
import scala.util.parsing.combinator._;
import scala.annotation.tailrec

object test4 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  val tok = new tokenizing                        //> tok  : ex10.cs.tokenizing = ex10.cs.tokenizing@7946e1f4
  
   tok.parsToTokens("(").head                     //> res0: (ex10.cs.test4.tok.token, String) = (Symbol,()
   val stringConstant = """(?s)\\Q(.*)\\E""".r    //> stringConstant  : scala.util.matching.Regex = (?s)\\Q(.*)\\E
 		  var s = """Keyboard.readInt("HOW MANY NUMBERS? ");"""
                                                  //> s  : String = Keyboard.readInt("HOW MANY NUMBERS? ");
 
 println(stringConstant replaceFirstIn(s, "Java"))//> Keyboard.readInt("HOW MANY NUMBERS? ");
   
}