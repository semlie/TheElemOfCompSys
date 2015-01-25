package ex10.cs
import scala.util.parsing.combinator._

class SimpleParser extends RegexParsers {
      def word: Parser[String]    = """[a-z]+""".r ^^ { _.toString }
      def digits:Parser[String]      = """[0-9]+""".r ^^ {_.toString+"d" }
      
    }
    
object cases  extends SimpleParser  {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

val KEYWORDS  = List("class", "constructor", "function", "method", "field", "static", "var", "int",
                "char", "boolean", "void", "true", "false", "null", "this", "let", "do", "if",
                "else", "while", "return")        //> KEYWORDS  : List[String] = List(class, constructor, function, method, field,
                                                  //|  static, var, int, char, boolean, void, true, false, null, this, let, do, if
                                                  //| , else, while, return)
val SYMBOLS = List('{', '}', '(', ')', '[', ']', '.', ',', ';', '+', '-', '*', '/', '&', '|', '<',
                '>', '=', '~')                    //> SYMBOLS  : List[Char] = List({, }, (, ), [, ], ., ,, ;, +, -, *, /, &, |, <,
                                                  //|  >, =, ~)
val kew = """(["class"|"constructor"|"function"|"method"|"field"|"static"|"var"|"int"|"char"|"boolean"|"void"|"true"|"false"|"null"|"this"|"let"|"do"|"if"|"else"|"while"|"return"])""".r
                                                  //> kew  : scala.util.matching.Regex = (["class"|"constructor"|"function"|"metho
                                                  //| d"|"field"|"static"|"var"|"int"|"char"|"boolean"|"void"|"true"|"false"|"null
                                                  //| "|"this"|"let"|"do"|"if"|"else"|"while"|"return"])
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

val rr = """\w+(?:\{\}\(\)\[\]\.\;\+\-\&\|\<\>\=\~)?""".r
                                                  //> rr  : scala.util.matching.Regex = \w+(?:\{\}\(\)\[\]\.\;\+\-\&\|\<\>\=\~)?
                                                  //| 
val c = rr.findAllIn(d).toList                    //> c  : List[String] = List(class, Main, Initializes, a, new, game, and, start
                                                  //| s, it, function, void, main, var, SquareGame, game)
c.foreach(println)                                //> class
                                                  //| Main
                                                  //| Initializes
                                                  //| a
                                                  //| new
                                                  //| game
                                                  //| and
                                                  //| starts
                                                  //| it
                                                  //| function
                                                  //| void
                                                  //| main
                                                  //| var
                                                  //| SquareGame
                                                  //| game
val cc = new SimpleParser                         //> cc  : ex10.cs.SimpleParser = ex10.cs.SimpleParser@3dd3bcd
println(parse(digits, "12434343 johnny come lately"))
                                                  //> [1.9] parsed: 12434343d
  }