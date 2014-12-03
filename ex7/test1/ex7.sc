package test1

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.MutableList
import scala.collection.mutable.Queue
//import org.specs._
object ex7 {
  val s = Array[String]()                         //> s  : Array[String] = Array()
  val s1 = "push constant 50"                     //> s1  : String = push constant 50
  val s2 = "push constant 30"                     //> s2  : String = push constant 30
  val s3 = "eq"                                   //> s3  : String = eq
  val s4 = "lt"                                   //> s4  : String = lt
 
  //Tuple3[String,String,String](s2.split(" "))
  // val a = s(0).split(" ")
  val l = List[String](s1, s2, s3,s4)             //> l  : List[String] = List(push constant 50, push constant 30, eq, lt)
  def stack = Map[Int, String]()                  //> stack: => scala.collection.immutable.Map[Int,String]
  var p = 256                                     //> p  : Int = 256
  var counter = 1                                 //> counter  : Int = 1
  def trueLabalCounter :Int = { counter += 1; counter/2 }
                                                  //> trueLabalCounter: => Int

  def pars(s: String) = {
    val a = s.split(" ")
    a.length match {
      case 3 => findOpration3(a(0), a(1), a(2))

      case 1 => findOpration1(a(0))
    }
  }                                               //> pars: (s: String)Any
  def moveStackPointer = {}                       //> moveStackPointer: => Unit
  def arrOp(o:String) = f"""@SP \nAM=M-1\nD=M\n@SP\nA=M-1\nM=D$o%sM\n"""
                                                  //> arrOp: (o: String)String
  def negOp = s"""@SP\nA=M-1\nM=-M\n"""           //> negOp: => String

  def compOper(o:String) = s"""@SP\nAM=M-1\nD=M\n@SP\nA=M-1\nD=M-D\nM=-1\n@TRUE$trueLabalCounter\nD;$o\n@SP\nA=M-1\nM=0\n(TRUE$trueLabalCounter)"""
                                                  //> compOper: (o: String)String
  def bitOper(o:String) = f"""@SP\nAM=M-1\nD=M\n@SP\nA=M-1\nM=D$o%sM"""
                                                  //> bitOper: (o: String)String
  def notOp = s"""@SP\nA=M-1\nM=!M\n"""           //> notOp: => String
  
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
 
  }                                               //> findOpration1: (s: String)String
  def findOpration3(op: String, sigment: String, offset: String) = {
    sigment match {
      case "constant" => s"""@$offset \nD=A"""
      case "push" => 0
      case "pop" => 0
    }
  }                                               //> findOpration3: (op: String, sigment: String, offset: String)Any
 
  for (x <- l)
    println(pars(x))                              //> @50 
                                                  //| D=A
                                                  //| @30 
                                                  //| D=A
                                                  //| @SP
                                                  //| AM=M-1
                                                  //| D=M
                                                  //| @SP
                                                  //| A=M-1
                                                  //| D=M-D
                                                  //| M=-1
                                                  //| @TRUE1
                                                  //| D;JEQ
                                                  //| @SP
                                                  //| A=M-1
                                                  //| M=0
                                                  //| (TRUE1)
                                                  //| @SP
                                                  //| AM=M-1
                                                  //| D=M
                                                  //| @SP
                                                  //| A=M-1
                                                  //| D=M-D
                                                  //| M=-1
                                                  //| @TRUE2
                                                  //| D;JLT
                                                  //| @SP
                                                  //| A=M-1
                                                  //| M=0
                                                  //| (TRUE2)

}