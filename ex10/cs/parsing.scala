package ex10.cs

import scala.util.parsing.combinator._;
import scala.annotation.tailrec

class parsing extends JavaTokenParsers {
  //base tokens
  val words = """(?s)[a-zA-Z]+\s(.*)""" //> words  : String = [a-zA-Z]+
  val symbol = """(?s)([\{\}\(\)\[\]\.\/\,\;\+\-\&\|\<\>\=\~])""".r
  val whiteSpaces = "(?s)\\s(.*)".r
  val keyword = """(?s)(class|constructor|function|method|field|static|var|int|char|boolean|void|true|false|null|this|let|do|if|else|while|return)""".r
//  val fCall = """(?s)([a-zA-Z]+)\.([a-zA-Z]+)\((.*)\)\s*\;(.*)""".r //Keyboard.readInt("HOW MANY NUMBERS? ");
  val stringConstant = """(?s)\"(.*?)\"""".r //> stringConstant  : scala.util.matching.Regex = \"(.*)\"
  val integerConstant = """(?s)([0-9]+)""".r //> digit  : scala.util.matching.Regex = ([0-9]+)
  val identifier = """(?s)([a-zA-Z_]+[0-9a-zA-Z_]*)""".r
  val mixed = """(?s)(%s|%s)(%s|%s)(.*)""".format(words, symbol, symbol, words).r

//  Program structure: 
  val classt = "class" ~ className ~ "{" ~ rep(classVarDec) ~ rep(subroutineDec) ~ "}"
  val classVarDec = ("static" | "field") ~ typeT ~ varName ~ rep("," ~ varName) ~ ";"
  val typeT = "int" | "char" | "boolean" | className
  val subroutineDec = ("constructor" | "function" | "method")  ~ ("void" | typeT) ~ subroutineName ~ "(" ~ parameterList ~ ")" ~ subroutineBody 
  val parameterList = ((typeT ~ varName) ~ rep("," ~ typeT ~ varName)*)?
  val subroutineBody = "{" ~ rep(varDec) ~ opt(statements)  ~ "}"
  val varDec = "var" ~ typeT ~ varName ~ rep("," ~ varName) ~ ";"
  val className = """[a-zA-Z_]+[0-9a-zA-Z_]*""".r //stringLiteral+ 
  val subroutineName = """[a-zA-Z_]+[0-9a-zA-Z_]*""".r //stringLiteral+
  val varName = """[a-zA-Z_]+[0-9a-zA-Z_]*""".r //stringLiteral+

  //Statements:
  def statements: Parser[Any] = rep(statement)
  def statement = letStatement ||| ifStatement ||| whileStatement ||| doStatement ||| ReturnStatement
  val letStatement = "let" ~ varName ~ opt("[" ~ expression ~ "]") ~ "=" ~ expression ~ ";"
  val ifStatement = "if" ~ "(" ~ expression ~ ")" ~ "{" ~ statements ~ "}" ~ rep1("else" ~ "{" ~ statements ~ "}")
  val whileStatement = "while" ~ "(" ~ expression ~ ")" ~ "{" ~ statements ~ "}"
  val doStatement = "do" ~ subroutineCall ~ ";"
  val ReturnStatement = "return" ~> (opt(expression) <~ ";")

  //Expressions=
  def expression: Parser[Any] = term ~ rep(op ~ term) // ^^ {  case ope ~ sll => println("OP "+ op + " "+ sll)  case on => println("no match "+ on)  }
  def term: Parser[Any] =  varName ~ "[" ~ expression  ~ "]" ^^{case x~y =>println("a="+x+" b="+y)}||| "(" ~ expression ~ ")" |||KeywordConstant ||| varName ||| subroutineCall ||| unaryOp ~ term ||| (integerConstant | stringConstant) ^^ { case x => println("found " + x) } 
  val subroutineCall =  (className | varName) ~ ("." ~> subroutineName) ~ ("(" ~> expressionList) <~ ")" ^^ { case subr ~ expe ~ ll => println("3")  } |||subroutineName ~ "(" ~ expressionList ~ ")" ^^ { case subr ~ "(" ~ expe ~ ")" => println("3") } 
  val expressionList = opt(expression ~ rep("," ~ expression))
  val op = "+" | "-" | "*" | "/" | "&" | "|" | "<" | ">" | "="
  val unaryOp = "-" | "~"
  val KeywordConstant = "true" | "false" | "null" | "this"

}