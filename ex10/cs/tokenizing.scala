package ex10.cs
import scala.util.parsing.combinator._;
import scala.annotation.tailrec

class tokenizing extends RegexParsers {

  sealed abstract class token
  case object Identifair extends token
  case object Symbol extends token
  case object Keyword extends token
  case object StringConstant extends token
  case object Digit extends token
  case object Err extends token

  
  def word: Parser[String]    = """[a-z]+""".r ^^ { _.toString }
  
  
  /**
   * Regex for macheing Tokens
   */
  val words = """(?s)[a-zA-Z]+\s(.*)""" //> words  : String = [a-zA-Z]+
  val symbol = """(?s)([\{\}\(\)\[\]\.\/\,\;\+\-\&\|\<\>\=\~])(.*)""".r
  val whiteSpaces = "(?s)\\s(.*)".r
  val keyword = """(?s)(class|constructor|function|method|field|static|var|int|char|boolean|void|true|false|null|this|let|do|if|else|while|return)(.*)""".r
  val fCall = """(?s)([a-zA-Z]+)\.([a-zA-Z]+)\((.*)\)\s*\;(.*)""".r //Keyboard.readInt("HOW MANY NUMBERS? ");
  val stringConstant = """(?s)\"(.*?)\"(.*)""".r //> stringConstant  : scala.util.matching.Regex = \"(.*)\"
  val digit = """(?s)([0-9]+)(.*)""".r //> digit  : scala.util.matching.Regex = ([0-9]+)
  val identifair = """(?s)([a-zA-Z_]+[0-9a-zA-Z_]*)(.*)""".r
  val mixed = """(?s)(%s|%s)(%s|%s)(.*)""".format(words, symbol, symbol, words).r
  val err = """(?s)(.)(.*)""".r

  def xkeyword(x: String) = """<keyword>%s</keyword>""".format(x)
  def xidentifier(l: String): String = """<identifier> %s </identifier>""".format(l)
  def xsymbol(l: String): String = """<symbol> %s </symbol>""".format(l)
  def xdigit(l: String): String = """<integerConstant> %s </integerConstant>""".format(l)
  def xstringConstant(l: String): String = """<stringConstant> %s </stringConstant>""".format(l)

  def getAllTokens(source: List[String]): Unit = {
    source.foreach(tokStrings(_))
  }
  def tokenCharsToListOfStrings(a: String): List[String] = {
    /**
     *
     */
    @tailrec def rec(acc: List[String] = List[String](), cur: String = "", an: Array[Char]): List[String] = {
      if (an.size == 0) acc
      else {
        val h = an.head.toString
        h match {
          case symbol() =>
            rec(acc ::: List(cur, an.head.toString), " ", an.tail)
          case words.r() => {
            rec(acc, cur + an.head, an.tail)
          }
          //          case x if x.matches("""\s""") && cur.matches("""\s""") => rec(acc, "", an.tail)
          case whiteSpace() => if (cur.matches("""\s"""))
            rec(acc, "", an.tail)
          else
            rec(acc ::: List(cur), "", an.tail)
          case _ => rec(acc ::: List(cur), "", an.tail)
        }
      }
    }
    rec(List(), "", a.toArray)
  }

  def tokStrings(s: String): Unit = {
    s match {
      case keyword(w) => println(xkeyword(w))
      case fCall(clas, fun, args) => getAllTokens(List[String](clas, ".", fun, "(", args, ")", ";"))
      case identifair(w) => println(xidentifier(w))
      case digit(w) => println(xdigit(w))
      case stringConstant(w) => println(xstringConstant(w))
      case mixed(w, s) => getAllTokens(List[String](w, s))
      case symbol(w) => println(xsymbol(w))
      case _ => getAllTokens(tokenCharsToListOfStrings(s))

    }
  }
  def parsToTokens(source: String): List[(token, String)] = {
    /**
     * parse all to tokens and output them in list of tuple (TYPE , STRING TOKEN)
     */
   @tailrec def findTokensBeRegex(acc: List[(token, String)], rest: String): List[(token, String)] = {
      if (rest == "") acc
      else {
        rest.trim() match {
          case keyword(w, r) => findTokensBeRegex((Keyword, w) :: acc, r)
     //     case fCall(clas, fun, args ,r) => findTokensBeRegex((Identifair, clas) ::(Symbol, ".")::(Identifair, fun) ::(Symbol, "(") ::(StringConstant, args) ::(Symbol, ")")::(Symbol, ";") :: acc, r) //getAllTokens(List[String](clas, ".", fun, "(", args, ")", ";"))
          case identifair(w,r) => findTokensBeRegex((Identifair, w) :: acc, r)
          case digit(w, r) => findTokensBeRegex((Digit, w) :: acc, r)
          case stringConstant(w,r) => findTokensBeRegex((StringConstant, w) :: acc, r)
          //case mixed(w, s) => getAllTokens(List[String](w, s))
          case symbol(w, r) => findTokensBeRegex((Symbol, w) :: acc, r)
          case err(w,r) =>  findTokensBeRegex((Err, w) :: acc, r)

        }
      }
    }
    findTokensBeRegex(List[(token, String)](), source).reverse
  }
  
  def WriteXmlTokens(l:List[(token, String)],fu: String => Any):Unit ={
    fu("<tokens>")
    for(term <- l){
      fu(matchParsseRole(term))
      
    }
    fu("</tokens>")
  }
  def matchParsseRole(t:(token, String)):String = {
    t match {
    case (Keyword, w) => xkeyword(w)
    case (Symbol, w) => xsymbol(w)
    case (Identifair, w) => xidentifier(w)
    case (StringConstant, w) => xstringConstant(w)
    case (Digit, w) => xdigit(w)
    case (Err,w) => ""
}
  }
}