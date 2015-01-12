/**
 * @author Admin
 *
 */
package ex8.cs
import scala.collection.mutable

class translator {

  var counter = 1
  var init = true
  def initial = init = false
  var scope = mutable.ListBuffer("")

  def trueLabalCounter: Int = { counter += 1; counter / 2 }

  def pars(s: String, file: String = "") = {
    val a = s.split(" ")
    a.length match {
      case 3 => findOpration3(a(0), a(1), a(2),file.split("//").last)
      case 2 => findOpration2(a(0), a(1))

      case 1 => findOpration1(a(0))
    }

  }

  //Arithmetic operator
  def arrOp(o: String) = f"""@SP \nAM=M-1\nD=M\n@SP\nA=M-1\nM=M$o%sD//end of arithmetic opp"""
  //Negative operator
  def negOp = s"""@SP\nA=M-1\nM=-M//end of neg op"""
  //comperation operator
  def compOper(o: String) = s"""@SP\nAM=M-1\nD=M\n@SP\nA=M-1\nD=M-D\nM=-1\n@TRUE$trueLabalCounter\nD;$o\n@SP\nA=M-1\nM=0\n(TRUE$trueLabalCounter)//end of comm opp"""
  //bitwise operator
  def bitOper(o: String) = f"""@SP\nAM=M-1\nD=M\n@SP\nA=M-1\nM=D$o%sM//end of bitwish opp"""
  //bitwise not operator
  def notOp = s"""@SP\nA=M-1\nM=!M\n//end of nor opp"""

  //push segments and operation
  //concat it in the end of each push order
  val pushCommanOrder = s"""@SP\nA=M\nM=D\n@SP\nM=M+1//end of push"""
  //for pointer temp and static
  def pushSigType(offset: String): String = s"""@$offset\nD=M\n""".concat(pushCommanOrder)
  //for arguments and location  this  that segment
  def pushSigArgsLoc(typ: String, offset: Int): String = s"""@$typ\nD=M\n@$offset\nA=D+A\nD=M\n""".concat(pushCommanOrder)
  def pushConstant(offset: String) = s"""@$offset \nD=A\n""".concat(pushCommanOrder)

  //pop segments and operation
  // for pointer temp and static
  def popSigType(offset: String): String = s"""@SP\nAM=M-1\nD=M\n@$offset\nM=D //pop tmp and static end"""
  //for arguments and location  this  that segment
  def popSigArgsLoc(typ: String, offset: Int): String = s"""@$typ\nD=M\n@$offset\nD=D+A\n@13\nM=D\n@SP\nAM=M-1\nD=M\n@13\nA=M\nM=D //pop arg end"""

  /**
   * to project 8
   */
  val scopename :String = if(scope.length>1)scope.last else ""
  def makeLabal(st: String): String = s"""(${scopename + "$" + st})"""
  val retFunction: String = {
    //clear this function from scope
     var scopelabale =scope.last
    scope -= scope.last

    def pointRetern(st: String): String = s"""@13\nAM=M-1\nD=M\n@$st\nM=D\n"""

    val point = List[String]("THAT", "THIS", "ARG", "LCL")

    s"""@LCL //farme and return point\nD=M\n@13\nM=D\n@5\nA=D-A\nD=M\n@14\nM=D\n""" +
      s"""@SP\nAM=M-1\nD=M\n@ARG\nA=M\nM=D // SP = ARG +1\n@ARG\nD=M+1\n@SP\nM=D\n""" + point.foldLeft("") {
        (acc, n) =>
          acc.concat(pointRetern(n))
      } +
      s"""@14\nA=M\n0;JMP//end of retern : $scopelabale"""
  }

  def goTo(l: String): String = s"""@${scopename + "$" + l}\n0;JMP"""
  def ifgoto(l: String): String = { s"""@SP\nAM=M-1\nD=M\n@${scopename+ "$" + l}\nD;JNE""" }
  def doFunction(fName: String, nArgs: String) = {
    //set the scope of this function
    scope += fName
    s"""($fName) //functoin:$fName\n""" + List.range(1, nArgs.toInt).foldLeft("") {
      (acc, x) =>
        acc + pars("push constant 0")
    }

  }
  def initilize: String = s"""@256\nD=A\n@SP\nM=D\n""" + pars("call Sys.init 0")
  def callFunction(fName: String, nArgs: String): String = {
    val labal = fName + "$"
    s"""@${labal + trueLabalCounter} //Start call function:$fName\nD=A\n@SP\nAM=M+1\nA=A-1\nM=D\n@LCL\nD=M\n@SP\nAM=M+1\nA=A-1\nM=D\n@ARG\nD=M\n@SP\nAM=M+1\nA=A-1\nM=D\n""" +
      s"""@THIS\nD=M\n@SP\nAM=M+1\nA=A-1\nM=D\n@THAT\nD=M\n@SP\nAM=M+1\nA=A-1\nM=D\n@SP\nD=M\n@${5 + nArgs.toInt}\nD=D-A\n@ARG\nM=D\n""" +
      s"""@SP\nD=M\n@LCL\nM=D\n@$fName\n0;JMP\n(${labal + trueLabalCounter}) //end of function:$fName"""
  }
  def findOpration1(s: String): String = {
    s match {
      case "add" => arrOp("+")
      case "sub" => arrOp("-")
      case "neg" => negOp
      case "eq" => compOper("JEQ")
      case "gt" => compOper("JGT")
      case "lt" => compOper("JLT")
      case "and" => bitOper("&")
      case "or" => bitOper("|")
      case "not" => notOp
      case "return" => retFunction

    }

  }
  def findOpration2(op: String, s: String): String = {
    op match {
      case "if-goto" => ifgoto(s)
      case "goto" => goTo(s)
      case "label" => makeLabal(s.toString())
    }
  }
  def findOpration3(op: String, sigment: String, offset: String,fileName :String =""): String = {
    op match {
      case "push" => pushInstrac(sigment, offset,fileName)
      case "pop" => popInstrac(sigment, offset,fileName)
      case "call" => callFunction(sigment, offset)
      case "function" => doFunction(sigment, offset)

    }

  }
  def popInstrac(sigment: String, offset: String,fileName:String=""): String =
    {
      sigment match {
        case "constant" => s"""@SP\nAM=M-1\nD=M\n@$offset\nM=D"""
        case "this" => popSigArgsLoc("THIS", offset.toInt)
        case "that" => popSigArgsLoc("THAT", offset.toInt)

        case "pointer" => popSigType((offset.toInt + 3).toString)
        case "temp" => popSigType((offset.toInt + 5).toString)
        case "static" => popSigType(fileName+"."+offset)

        case "argument" => popSigArgsLoc("ARG", offset.toInt)
        case "local" => popSigArgsLoc("LCL", offset.toInt)
      }
    }

  def pushInstrac(sigment: String, offset: String,fileName:String=""): String =
    {
      sigment match {
        case "constant" => pushConstant(offset)
        case "this" => pushSigArgsLoc("THIS", offset.toInt)
        case "that" => pushSigArgsLoc("THAT", offset.toInt)
        case "pointer" => pushSigType((offset.toInt + 3).toString)
        case "temp" => pushSigType((offset.toInt + 5).toString)
        case "static" => pushSigType(fileName+"."+offset)
        case "argument" => pushSigArgsLoc("ARG", offset.toInt)
        case "local" => pushSigArgsLoc("LCL", offset.toInt)
      }

    }

  def transAll(file: String, arr: Array[String], f: (String, String) => String,fileName:String) = {
    if (this.init) {
      helpers.writeTranslatToFile(file, initilize)
      this.initial
    }
    for (x <- arr filterNot (_ == "")) {
      helpers.writeTranslatToFile(file, f(x.replaceAll("//.*", ""), fileName))
    }
  }

}