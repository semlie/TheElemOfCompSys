package ex10.cs
import scala.util.parsing.combinator._
import org.scalatest._
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import scala.collection.mutable.Stack

@RunWith(classOf[JUnitRunner])
class ExampleSpec extends FlatSpec with Matchers {
  val tok = new tokenizing
  var s = """Keyboard.readInt("HOW MANY NUMBERS? "); """
    
    "Can open File" should "chack if get file list and get  " in {
    val dir = helpers.fileList("C:/tmp/p10/ArrayTest")
    dir(0).toString() should  be("C:/tmp/p10/ArrayTest/Main.jack".toString())
    val fileString = helpers.allFileLines(dir(0))
     var m = """class Main"""
    assert(fileString.contains(m))
    
  }
  "A Tokenizing" should "it check if pars good in recurtion" in {
    val d = tok.parsToTokens("( var d)")
    d.head should be((tok.Symbol, "("))
    d(1) should be((tok.Keyword, "var"))
    d(2) should be((tok.Identifair, "d"))
    d(3) should be((tok.Symbol, ")"))
    // tok.parsToTokens("( var d")(2) should be((tok.Identifair,"d"))

  }

  it should "it check if find constantString" in {
    val d = tok.parsToTokens(s)
    d.head should be((tok.Identifair,"Keyboard"))
    d.last should be((tok.Symbol,";"))

  }
  "A xml role is " should "be right xml for any role" in {
    val p = tok.parsToTokens("( var d)")
    
    
    tok.matchParsseRole(p(0)) should be(tok.xsymbol("("))
  
  } 
  

}
//class ExampleSuite extends FunSuite {
//   test("pars  fun call with String args") {
// 		  val tok = new tokenizing
//		  var s = """Keyboard.readInt("HOW MANY NUMBERS? ");"""
//		 val result =  tok.getAllTokens(List(s))  
//   
//    assert(result === 2)
//    assert(stack.size === oldSize - 2)
//  }
