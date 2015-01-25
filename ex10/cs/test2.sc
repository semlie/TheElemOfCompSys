package ex10.cs
import scala.util.parsing.combinator._

class SimpleParser1 extends RegexParsers {
  def keyword = """\w""".r
  def identifier: Parser[String] = """[0-9]+""".r
  val symbolN = """\w+(?:\{\}\(\)\[\]\.\;\+\-\&\|\<\>\=\~)?""".r
  def symbol = """(\{\}\(\)\[\]\.\;\+\-\&\|\<\>\=\~)?""".r
  def mixed = keyword ~ symbol

  sealed abstract class Tokenaizer
  case object keywords extends Tokenaizer

}
object test2 extends SimpleParser1 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  val comment = """(?:\/\*\*.+\*\/)""".r          //> comment  : scala.util.matching.Regex = (?:\/\*\*.+\*\/)
  val keyword = """\w""".r                        //> keyword  : scala.util.matching.Regex = \w
  def identifier: Parser[String] = """[0-9]+""".r //> identifier: => ex10.cs.test2.Parser[String]
  val symbolN = """\w+(?:\{\}\(\)\[\]\.\;\+\-\&\|\<\>\=\~)?""".r
                                                  //> symbolN  : scala.util.matching.Regex = \w+(?:\{\}\(\)\[\]\.\;\+\-\&\|\<\>\=\
                                                  //| ~)?
  val symbol = """(\{\}\(\)\[\]\.\;\+\-\&\|\<\>\=\~)?""".r
                                                  //> symbol  : scala.util.matching.Regex = (\{\}\(\)\[\]\.\;\+\-\&\|\<\>\=\~)?
  val mixed = keyword ~ symbol                    //> mixed  : ex10.cs.test2.Parser[ex10.cs.test2.~[String,String]] = Parser (~)

  val d = """class Main {

    /** Initializes a new game and starts it. */
    function void main() {
        var SquareGame game;
"""                                               //> d  : String = "class Main {
                                                  //| 
                                                  //|     /** Initializes a new game and starts it. */
                                                  //|     function void main() {
                                                  //|         var SquareGame game;
                                                  //| "
  val ss1 = d.toArray.filterNot(_ == " ")         //> ss1  : Array[Char] = Array(c, l, a, s, s,  , M, a, i, n,  , {, 
                                                  //| , 
                                                  //| ,  ,  ,  ,  , /, *, *,  , I, n, i, t, i, a, l, i, z, e, s,  , a,  , n, e, w,
                                                  //|   , g, a, m, e,  , a, n, d,  , s, t, a, r, t, s,  , i, t, .,  , *, /, 
                                                  //| ,  ,  ,  ,  , f, u, n, c, t, i, o, n,  , v, o, i, d,  , m, a, i, n, (, ),  ,
                                                  //|  {, 
                                                  //| ,  ,  ,  ,  ,  ,  ,  ,  , v, a, r,  , S, q, u, a, r, e, G, a, m, e,  , g, a,
                                                  //|  m, e, ;, 
                                                  //| )
  val ss = symbolN.findAllIn(d).toArray           //> ss  : Array[String] = Array(class, Main, Initializes, a, new, game, and, sta
                                                  //| rts, it, function, void, main, var, SquareGame, game)
  def recIt(a: Array[Char]): List[String] = {

    def rec(acc: List[String] = List[String](), cur: String = "", an: Array[Char]):List[String]= {
      if (an.size == 0) acc
      else {
        an.head  match {
          case keyword => {
            rec(acc, cur + an.head, an.tail)
          }
          case symbol =>
            rec(acc ::: List(cur, an.head.toString), " ", an.tail)
            
        }
      }
    }
    rec(List(),"",a)
  }                                               //> recIt: (a: Array[Char])List[String]
 println( recIt(ss1))                             //> List()
  // parseAll(d)
  def ex(s: Any) = {
    // s.foreach { x =>
    s match {
      case symbol(s) => println("keywords :" + s)
      case _ => println("asasa")
      //   case mixed(s) => println("mix :" + x)
      //  }
    }

  }                                               //> ex: (s: Any)Unit
  ex(d)                                           //> asasa
  
    def allFileLines(file: String) = io.Source.fromFile(file).getLines.filterNot(_.startsWith("//")).mkString
                                                  //> allFileLines: (file: String)String
    def we =allFileLines("C:/tmp/p10/ArrayTest/Main.jack").replaceAll("""(?:\/\*\*.+\*\/)""","").split("\\s").filterNot(_ =="")
                                                  //> we: => Array[String]
             we                                   //> res0: Array[String] = Array(class, Main, {, function, void, main(), {, var,
                                                  //|  Array, a;, var, int, length;, var, int, i,, sum;, let, length, =, Keyboard
                                                  //| .readInt("HOW, MANY, NUMBERS?, ");, let, a, =, Array.new(length);, let, i, 
                                                  //| =, 0;, while, (i, <, length), {, let, a[i], =, Keyboard.readInt("ENTER, THE
                                                  //| , NEXT, NUMBER:, ");, let, i, =, i, +, 1;, }, let, i, =, 0;, let, sum, =, 0
                                                  //| ;, while, (i, <, length), {, let, sum, =, sum, +, a[i];, let, i, =, i, +, 1
                                                  //| ;, }, do, Output.printString("THE, AVERAGE, IS:, ");, do, Output.printInt(s
                                                  //| um, /, length);, do, Output.println();, return;, }})
}