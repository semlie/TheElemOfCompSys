package ex10.cs

import scala.util.parsing.combinator._
import java.util.regex._
object test3 extends RegexParsers {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  val words = """[a-zA-Z]+""".r                   //> words  : scala.util.matching.Regex = [a-zA-Z]+
  val symbol = """[\{|\}|\(|\)|\[|\]|\.|\;|\+|\-|\&|\||\<|\>|\=|\~]""".r
                                                  //> symbol  : scala.util.matching.Regex = [\{|\}|\(|\)|\[|\]|\.|\;|\+|\-|\&|\||\
                                                  //| <|\>|\=|\~]
  val stringConstant = """\"(.*)\"""".r           //> stringConstant  : scala.util.matching.Regex = \"(.*)\"
  val digit = """([0-9]+)""".r                    //> digit  : scala.util.matching.Regex = ([0-9]+)
  val identifair = """([a-zA-Z_]+[0-9a-zA-Z_]*)""".r
                                                  //> identifair  : scala.util.matching.Regex = ([a-zA-Z_]+[0-9a-zA-Z_]*)

  //  var s = """Keyboard.readInt("HOW MANY NUMBERS? ");"""
  var s = "\" fdlklfsdkl \""                      //> s  : String = " fdlklfsdkl "

  val mixed = """(%s|%s)(%s|%s)""".format(words, symbol, symbol, words).r
                                                  //> mixed  : scala.util.matching.Regex = ([a-zA-Z]+|[\{|\}|\(|\)|\[|\]|\.|\;|\+|
                                                  //| \-|\&|\||\<|\>|\=|\~])([\{|\}|\(|\)|\[|\]|\.|\;|\+|\-|\&|\||\<|\>|\=|\~]|[a-
                                                  //| zA-Z]+)
  val fCall = """(?m)([a-zA-Z]+)\.([a-zA-Z]+)\((.*)\)\s*\;""".r //Keyboard.readInt("HOW MANY NUMBERS? ");
                                                  //> fCall  : scala.util.matching.Regex = (?m)([a-zA-Z]+)\.([a-zA-Z]+)\((.*)\)\s*
                                                  //| \;
  val keyword = """(?m)(class|constructor|function|method|field|static|var|int|char|boolean|void|true|false|null|this|let|do|if|else|while|return)\s(.*|\n*)""".r
                                                  //> keyword  : scala.util.matching.Regex = (?m)(class|constructor|function|metho
                                                  //| d|field|static|var|int|char|boolean|void|true|false|null|this|let|do|if|else
                                                  //| |while|return)\s(.*|\n*)

  def xkeyword(x: String) = """<keyword>%s</keyword>""".format(x)
                                                  //> xkeyword: (x: String)String
 // def xfCall = (l: String) => """<identifier>%s</identifier>\n<symbol>.</symbol>\n<identifier>%s</identifier>\n<symbol> ( </symbol><stringConstant>%s</stringConstant>\n<symbol> ) </symbol>""".format(s)
  def xidentifier(l: String): String = """<identifier> %s </identifier>""".format(s)
                                                  //> xidentifier: (l: String)String

  def getAllTokens(source: List[String]): Unit = {
    source.foreach(tokStrings(_))
  }                                               //> getAllTokens: (source: List[String])Unit

  def tokStrings(s: String): Unit = {
    s match {
      case keyword(w ,r) => println("""<keyword>%s</keyword>""".format(w))
      case fCall(clas, fun, args) => getAllTokens(List[String](clas, ".", fun, "(", args, ")", ";"))
      case mixed(w, s) => getAllTokens(List[String](w, s))
      case identifair(w) => println("word = " + w)
      case digit(w) => println("digit = " + w)
      case stringConstant(w) => println("string = " + w)
      case _ => println("no match")
    }
  }                                               //> tokStrings: (s: String)Unit
  tokStrings("""var\n -netretoert\n""")           //> no match
  s.matches(fCall.toString)                       //> res0: Boolean = false
  fCall.toString                                  //> res1: String = (?m)([a-zA-Z]+)\.([a-zA-Z]+)\((.*)\)\s*\;

  "ss".to                                         //> res2: scala.collection.immutable.IndexedSeq[Char] = Vector(s, s)

}