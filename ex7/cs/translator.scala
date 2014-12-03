/**
 * @author Admin
 *
 */
package ex7.cs

class translator {


  var counter = 1
  def trueLabalCounter :Int = { counter += 1; counter/2 }

  def pars(s: String) = {
    val a = s.split(" ")
    a.length match {
      case 3 => findOpration3(a(0), a(1), a(2))

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
  def pushSigType(offset: Int): String = s"""@$offset\nD=M\n""".concat(pushCommanOrder)
  //for arguments and location  this  that segment
  def pushSigArgsLoc(typ: String, offset: Int): String = s"""@$typ\nD=M\n@$offset\nA=D+A\nD=M\n""".concat(pushCommanOrder)
def pushConstant(offset:String) = s"""@$offset \nD=A\n""".concat(pushCommanOrder)
  
  //pop segments and operation
  // for pointer temp and static
  def popSigType(offset: Int): String = s"""@SP\nAM=M-1\nD=M\n@$offset\nM=D//pop tmp and static end"""
    //for arguments and location  this  that segment
  def popSigArgsLoc(typ: String, offset: Int): String = s"""@$typ\nD=M\n@$offset\nD=D+A\n@13\nM=D\n@SP\nAM=M-1\nD=M\n@13\nA=M\nM=D//pop arg end"""


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
    }

  }
  def findOpration3(op: String, sigment: String, offset: String): String = {
    op match {
      case "push" => pushInstrac(sigment, offset)
      case "pop" => popInstrac(sigment, offset)
    }

  }
  def popInstrac(sigment: String, offset: String): String =
    {
      sigment match {
        case "constant" => s"""@SP\nAM=M-1\nD=M\n@$offset\nM=D"""
        case "this" => popSigArgsLoc("THIS",offset.toInt)
        case "that" => popSigArgsLoc("THAT",offset.toInt)
        
        case "pointer" => popSigType(offset.toInt + 3)
        case "temp" => popSigType(offset.toInt + 5)
        case "static" => popSigType(offset.toInt + 16)
        
        case "argument" => popSigArgsLoc("ARG", offset.toInt)
        case "local" => popSigArgsLoc("LCL", offset.toInt)
      }
    }

  def pushInstrac(sigment: String, offset: String): String =
    {
      sigment match {
        case "constant" => pushConstant(offset)
        case "this" => pushSigArgsLoc("THIS", offset.toInt)
        case "that" => pushSigArgsLoc("THAT", offset.toInt)
        case "pointer" => pushSigType(offset.toInt + 3)
        case "temp" => pushSigType(offset.toInt + 5)
        case "static" => pushSigType(offset.toInt + 16)
        case "argument" => pushSigArgsLoc("ARG", offset.toInt)
        case "local" => pushSigArgsLoc("LCL", offset.toInt)
      }

    }

  def transAll(file: String, arr: Array[String], f: String => String) = {
    for (x <- arr filterNot (_ == "")) {
      helpers.writeTranslatToFile(file, f(x))
    }
  }

}