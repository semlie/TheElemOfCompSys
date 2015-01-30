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
  val pars = new parsing
  "Can open File" should "chack if get file list and get  " in {
    val dir = helpers.fileList("C:/tmp/p10/ArrayTest")
    dir(0).toString() should be("C:/tmp/p10/ArrayTest/Main.jack".toString())
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
    d.head should be((tok.Identifair, "Keyboard"))
    d.last should be((tok.Symbol, ";"))

  }
  "A xml role is " should "be right xml for any role" in {
    val p = tok.parsToTokens("( var d)")

    tok.matchParsseRole(p(0)) should be(tok.xsymbol("("))

  }
  " A test for KeywordConstant" should " KeywordConstant true" in {
    val stream = """true"""
    pars.parseAll(pars.expression, stream).successful should be(true)

  }
  " A test for parser combinator while statmant" should " read a While  simple statmant and return AST" in {
    val stream = """while(x > 0) {return x; }"""
    pars.parseAll(pars.statements, stream).successful should be(true)

  }
  " A test for parser combinator let statmant" should "let i = i + 1;" in {
    val stream = """let i = i + 1;"""
    pars.parseAll(pars.statements, stream).successful should be(true)

  }
  " A test for parser combinator let statmant with array" should " let sum = sum + a[i];" in {
    val stream = """let sum = sum + a[i];"""
    pars.parseAll(pars.statements, stream).successful should be(true)

  }
  " A test for  Array.new(length);" should "Array.new(length);" in {
    val stream = """Array.new(length)"""
    pars.parseAll(pars.expression, stream).successful should be(true)

  }
  " A test for let a = Array.new(length);" should "let a = Array.new(length);" in {
    val stream = """let a = Array.new(length);"""
    pars.parseAll(pars.statement, stream).successful should be(true)

  }
  " A test for varDec" should " must return true" in {
    val stream = """var int sum;"""
    pars.parseAll(pars.varDec, stream).successful should be(true)

  }
  " A test for multi varDec" should "must reatern true" in {
    val stream = """var int sum,ler;"""
    pars.parseAll(pars.varDec, stream).successful should be(true)

  }
  " A test for let sum = sum + 1;" should " return true if its pars OK" in {
    val stream = """let sum = sum + 1;"""
    pars.parseAll(pars.statements, stream).successful should be(true)

  }
  " A test for parser combinator  simple expression " should " read c simple expression a+b" in {
    val stream = """a + b"""
    pars.parseAll(pars.expression, stream).successful should be(true)

  }
  " A test for   let a[ i ] = 0;" should " return true if its pars OK" in {
    val stream = """let a[ i ] = 0;"""

    pars.parseAll(pars.statement, stream).successful should be(true)

  }
  " A test for a[i]" should " return true if its pars OK" in {
    val stream = """a[i]"""
    pars.parseAll(pars.term, stream).successful should be(true)

  }
  " A test for parser combinator  array expretion expression " should " array expression sum + a[i];" in {
    val stream = """ return a[i];"""
    pars.parseAll(pars.statement, stream).successful should be(true)

  }
  " A test DO statmant " should " 	do Output.printInt(sum / length);" in {
    val stream = """ do Output.printInt(sum / length);"""
    pars.parseAll(pars.statement, stream).successful should be(true)

  }
  " A test class main" should " class main {}" in {
    val stream = """ class main{
			}
			  """
    pars.parseAll(pars.classt, stream).successful should be(true)

  }
  " A test subroutineBody statmant " should " subroutineBody {} test" in {
    val stream = """{
        var Array a;
        var int length;
    		var int i, sum;}
	"""
    pars.parseAll(pars.subroutineBody, stream).successful should be(true)
  }
  " A test function statmant " should " function void main() {}" in {
    val stream = """ function void main() {
			  var Array a;
			  var int length;
			  var int i, sum;}
			  """
    pars.parseAll(pars.subroutineDec, stream).successful should be(true)

  }
  " A test read file " should "read and parss al file" in {
	       val stream = helpers.allFileLines("C:/tmp/p10/ArrayTest/Main.jack")

			  pars.parseAll(pars.classt, stream).successful should be(true)
	  
  }

}

//class ExampleSuite extends FunSuite {
//   test("pars  fun call with String args") {
// 		  val to	k = new tokenizing
//		  var s = """Keyboard.readInt("HOW MANY NUMBERS? ");"""
//		 val result =  tok.getAllTokens(List(s))  
//   
//    assert(result === 2)
//    assert(stack.size === oldSize - 2)
//  }
