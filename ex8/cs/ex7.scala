package ex8.cs

class ex7 {
  var counter = 0 
  def trueLabalCounter = { counter += 1 } 

  def parser(s: String) = {
    val a = s.split(" ")
    a.length match {
      case 3 => findOpration3(a(0), a(1), a(2))

      case 1 => findOpration1(a(0))
    }
  } 
  def moveStackPointer = {} 
  def arrOp(o: String) = f"""@SP \nAM=M-1\nD=M\n@SP\nA=M-1\nM=D$o%sM\n"""

  def negOp = s"""@SP\nA=M-1\nM=-M\n""" 

  def compOper(o: String) = s"""@SP\nAM=M-1\nD=M\n@SP\nA=M-1\nD=M-D\nM=-1\n@TRUE$counter\nD;$o\n@SP\nA=M-1\nM=0\n(TRUE$counter)"""; trueLabalCounter

  def bitOper(o: String) = f"""@SP\nAM=M-1\nD=M\n@SP\nA=M-1\nM=D$o%sM"""

  def notOp = s"""@SP\nA=M-1\nM=!M\n""" 

  def findOpration1(s: String) = {
    s match {
      case "add" => arrOp("+")
      case "sub" => arrOp("-")
      case "neg" => negOp
      case "eq" => compOper("JEQ")
      case "gt" => compOper("JGT")
      case "lt" => compOper("JLT")
      case "and" => bitOper("&")
      case "or" => bitOper("|")

    }
  } 
  def findOpration3(op: String, sigment: String, offset: String) = {
    sigment match {
      case "constant" => s"""@$offset \nD=A"""
      case "push" => 0
      case "pop" => 0
    }
  }
}