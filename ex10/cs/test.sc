package ex10.cs

object test {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  import scala.util.matching.Regex
    // definitions
  val number = """(\d+(\.\d+)?)""".r              //> number  : scala.util.matching.Regex = (\d+(\.\d+)?)
  val product = """[\*\/]""".r                    //> product  : scala.util.matching.Regex = [\*\/]
  val term = """[\+\-]""".r                       //> term  : scala.util.matching.Regex = [\+\-]
  val exponent = """\^""".r                       //> exponent  : scala.util.matching.Regex = \^
  val negation = """\!""".r                       //> negation  : scala.util.matching.Regex = \!
  val leftParens = """\(""".r                     //> leftParens  : scala.util.matching.Regex = \(
  val rightParens = """\)""".r                    //> rightParens  : scala.util.matching.Regex = \)
  
  type Token = (TokenType, String)
  type Counts = scala.collection.mutable.Map[TokenType, Int]
  
  abstract class TokenType
  case object IntegerType extends TokenType
  case object FloatingPointType extends TokenType
  case object CharacterType extends TokenType
  case object StringType extends TokenType
  case object IdType extends TokenType
  case object KeywordType extends TokenType
  case object SymbolType extends TokenType
  case object CommentType extends TokenType
  case object DelimiterType extends TokenType
  case object EofType extends TokenType
  case object ErrorType extends TokenType
  
   val Eof = ((EofType, ""), "")                  //> Eof  : ((ex10.cs.test.EofType.type, String), String) = ((EofType,""),"")
    val decimalNumeral = """0|[1-9][0-9]*"""      //> decimalNumeral  : String = 0|[1-9][0-9]*
  val hexNumeral = "0[xX][0-9a-fA-F]+"            //> hexNumeral  : String = 0[xX][0-9a-fA-F]+
  val octalNumeral = "0[0-7]+"                    //> octalNumeral  : String = 0[0-7]+

  val exponentPart = """[Ee][+-]?\d+"""           //> exponentPart  : String = [Ee][+-]?\d+
  val floatType = """[FfDd]"""                    //> floatType  : String = [FfDd]
  val fp1 = """\d+\.\d*(%s)?%s?""".format(exponentPart, floatType)
                                                  //> fp1  : String = \d+\.\d*([Ee][+-]?\d+)?[FfDd]?
  val fp2 = """\.\d+(%s)?%s?""".format(exponentPart, floatType)
                                                  //> fp2  : String = \.\d+([Ee][+-]?\d+)?[FfDd]?
  val fp3 = """\d+(%s)%s?""".format(exponentPart, floatType)
                                                  //> fp3  : String = \d+([Ee][+-]?\d+)[FfDd]?
  val fp4 = """\d+(%s)?%s""".format(exponentPart, floatType)
                                                  //> fp4  : String = \d+([Ee][+-]?\d+)?[FfDd]

  val upper = """[A-Z$_]"""                       //> upper  : String = [A-Z$_]
  val lower = """[a-z]"""                         //> lower  : String = [a-z]
  val letter = """[a-zA-Z$_]"""                   //> letter  : String = [a-zA-Z$_]
  val op = """[!#$%&*+-/:<=>?@\^|~]+""" // XXX do I need to escape some chars?
                                                  //> op  : String = [!#$%&*+-/:<=>?@\^|~]+
  val idrest = """[a-zA-Z$_0-9]*(_%s)?""".format(op)
                                                  //> idrest  : String = [a-zA-Z$_0-9]*(_[!#$%&*+-/:<=>?@\^|~]+)?
  val varid = lower + idrest                      //> varid  : String = [a-z][a-zA-Z$_0-9]*(_[!#$%&*+-/:<=>?@\^|~]+)?
  val plainid = """(%s%s|%s|%s)""".format(upper, idrest, varid, op)
                                                  //> plainid  : String = ([A-Z$_][a-zA-Z$_0-9]*(_[!#$%&*+-/:<=>?@\^|~]+)?|[a-z][
                                                  //| a-zA-Z$_0-9]*(_[!#$%&*+-/:<=>?@\^|~]+)?|[!#$%&*+-/:<=>?@\^|~]+)
  val id = """(?s)(%s|`[^`]+`)(.*)""".format(plainid)
                                                  //> id  : String = (?s)(([A-Z$_][a-zA-Z$_0-9]*(_[!#$%&*+-/:<=>?@\^|~]+)?|[a-z][
                                                  //| a-zA-Z$_0-9]*(_[!#$%&*+-/:<=>?@\^|~]+)?|[!#$%&*+-/:<=>?@\^|~]+)|`[^`]+`)(.*
                                                  //| )

  val SkipWhitespace = """(?s)\s+(.*)""".r        //> SkipWhitespace  : scala.util.matching.Regex = (?s)\s+(.*)
  val IntegerLiteral = """(?s)(((%s)|%s|%s)([Ll]?))(.*)""".format(hexNumeral, octalNumeral, decimalNumeral).r
                                                  //> IntegerLiteral  : scala.util.matching.Regex = (?s)(((0[xX][0-9a-fA-F]+)|0[0
                                                  //| -7]+|0|[1-9][0-9]*)([Ll]?))(.*)
  val FloatingPointLiteral = "(?s)(%s|%s|%s|%s)(.*)".format(fp1, fp2, fp3, fp4).r
                                                  //> FloatingPointLiteral  : scala.util.matching.Regex = (?s)(\d+\.\d*([Ee][+-]?
                                                  //| \d+)?[FfDd]?|\.\d+([Ee][+-]?\d+)?[FfDd]?|\d+([Ee][+-]?\d+)[FfDd]?|\d+([Ee][
                                                  //| +-]?\d+)?[FfDd])(.*)
  val CharacterLiteral = """(?s)('.'|'\\[nbtfr'"\\]'|'\\[0-7]+'|'\\u[0-9a-fA-F]+')(.*)""".r
                                                  //> CharacterLiteral  : scala.util.matching.Regex = (?s)('.'|'\\[nbtfr'"\\]'|'\
                                                  //| \[0-7]+'|'\\u[0-9a-fA-F]+')(.*)
  val Id = id.r                                   //> Id  : scala.util.matching.Regex = (?s)(([A-Z$_][a-zA-Z$_0-9]*(_[!#$%&*+-/:<
                                                  //| =>?@\^|~]+)?|[a-z][a-zA-Z$_0-9]*(_[!#$%&*+-/:<=>?@\^|~]+)?|[!#$%&*+-/:<=>?@
                                                  //| \^|~]+)|`[^`]+`)(.*)
  val Delimiter = """(?s)([;,.()\[\]{}])(.*)""".r //> Delimiter  : scala.util.matching.Regex = (?s)([;,.()\[\]{}])(.*)
  
  //val StringLiteral = "(\"\"\".??\"\"\"|\"((\\\"|[^\"])*\"))(.*)".r
  val StringLiteral1 = "(?s)(\\\"\\\"\\\".*?\\\"\\\"\\\")(.*)".r
                                                  //> StringLiteral1  : scala.util.matching.Regex = (?s)(\"\"\".*?\"\"\")(.*)
//  val StringLiteral2 = """(?s)(".*?")(.*)""".r
  val StringLiteral2 = "(?s)(\"(\\\\.|.*?)*\")(.*)".r
                                                  //> StringLiteral2  : scala.util.matching.Regex = (?s)("(\\.|.*?)*")(.*)
      
      
  val SymbolLiteral = "(?s)'%s(.*)".format(plainid).r
                                                  //> SymbolLiteral  : scala.util.matching.Regex = (?s)'([A-Z$_][a-zA-Z$_0-9]*(_[
                                                  //| !#$%&*+-/:<=>?@\^|~]+)?|[a-z][a-zA-Z$_0-9]*(_[!#$%&*+-/:<=>?@\^|~]+)?|[!#$%
                                                  //| &*+-/:<=>?@\^|~]+)(.*)

 def nextToken(text: String): (Token, String) = {
    if (text startsWith "//") {
      val endOfComment = text indexOf "\n"
      if (endOfComment == -1) ((CommentType, text), "")
      else ((CommentType, text.substring(0, endOfComment)), text.substring(endOfComment))
    } else if (text startsWith "/*") {
      val endOfComment = text indexOf "*/"
      if (endOfComment == -1) ((ErrorType, text), "")
      else ((CommentType, text.substring(0, endOfComment + 2)), text.substring(endOfComment + 2))
    } else
      text match {
        case "" => Eof
        case SkipWhitespace(rest) => nextToken(rest)
        case FloatingPointLiteral(token, _, _, _, _, rest) => ((FloatingPointType, token), rest)
        case IntegerLiteral(token, _, _, _, rest) => ((IntegerType, token), rest)
        case CharacterLiteral(token, rest) => ((CharacterType, token), rest)
        //    case SingleLineComment(token, _, rest) => ((CommentType, token), rest)
       // case Id(token, _, _, _, rest) => ""// if (keywords contains token) ((KeywordType, token), rest) else ((IdType, token), rest)
        //     case Delimiter(token, rest) => ((KeywordType, token), rest)
        case SymbolLiteral(token, _, _, rest) => ((SymbolType, token), rest)
        case StringLiteral1(token, rest)      => ((StringType, token), rest)
        case StringLiteral2(token, _, rest)   => ((StringType, token), rest)
        case rest => {
          val firstChar = rest(0)
          if ("()[]{};,." contains firstChar) ((DelimiterType, firstChar.toString), rest.substring(1))
          else if ("\u21D2\u2190" contains firstChar) ((KeywordType, firstChar.toString), rest.substring(1))
          else ((ErrorType, rest), rest.substring(1))
        }
      }
  }                                               //> nextToken: (text: String)(ex10.cs.test.Token, String)


}