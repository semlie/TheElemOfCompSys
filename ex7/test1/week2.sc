package test1

import java.util.stream.Collector

object week2 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  def sum(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a == b) acc
      else loop(a + 1, f(a) + acc)
    }
    loop(a, 0)
  }                                               //> sum: (f: Int => Int)(a: Int, b: Int)Int

  val v = sum(x => x * x)(_, _)                   //> v  : (Int, Int) => Int = <function2>
  v(1, 5)                                         //> res0: Int = 30

  def prodact(f: Int => Double)(a: Int, b: Int): Double = {
    if (a > b) 1 else f(a) * prodact(f)(a + 1, b)
  }                                               //> prodact: (f: Int => Double)(a: Int, b: Int)Double
  prodact(x => x * x)(3, 4)                       //> res1: Double = 144.0

  def fact(n: Int): Double = prodact(x => x)(1, n)//> fact: (n: Int)Double

  fact(150)                                       //> res2: Double = 5.7133839564458575E262

  def diver(x: Int, y: Int): Boolean = { if ((y % x) == 0) true else false }
                                                  //> diver: (x: Int, y: Int)Boolean
  //diver (2 ,3)
  def gdc(x: Int, y: Int):Int = {
    def gdc3(x: Int, y: Int, z: Int): Int = {
      if (diver(z, x)&&diver(z, y)) z else gdc3(x, y,z-1)
    }
    gdc3(x,y,x/2)
  }                                               //> gdc: (x: Int, y: Int)Int
  gdc(8, 32)                                      //> res3: Int = 4
 

import scala.collection.mutable.Stack
val a = new Stack[Int]                            //> a  : scala.collection.mutable.Stack[Int] = Stack()
for(x <- 1 to 10) {a.push(x)}
a                                                 //> res4: scala.collection.mutable.Stack[Int] = Stack(10, 9, 8, 7, 6, 5, 4, 3, 2
                                                  //| , 1)
}