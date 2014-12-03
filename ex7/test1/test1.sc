package test1

object test1 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  val x = 1                                       //> x  : Int = 1
  def increase(i: Int) = i * i                    //> increase: (i: Int)Int
  increase(x)                                     //> res0: Int = 1
  import java.nio.file.{ Files, Path }
  val path = "c://"                               //> path  : String = c://
  val a = new java.io.File(path).listFiles.filter(_.getName.endsWith(".log"))
                                                  //> a  : Array[java.io.File] = Array(c:\M1319.log)
  def scalaFiles =
    for {
      file <- a

    } println(file.getPath())                     //> scalaFiles: => Unit
  scalaFiles                                      //> c:\M1319.log

  val alice = new Person("Alice", 25)             //> alice  : test1.test1.Person = Person(Alice,25)
  val bob = new Person("Bob", 32)                 //> bob  : test1.test1.Person = Person(Bob,32)
  val charlie = new Person("Charlie", 32)         //> charlie  : test1.test1.Person = Person(Charlie,32)
  val charli1 = new Person("Charlie", 323)        //> charli1  : test1.test1.Person = Person(Charlie,323)

  val cie = new pi(12, 32, "Sem")                 //> cie  : test1.test1.pi = pi(12,32,Sem)

  for (person <- List(alice, bob,charli1, charlie, cie)) {
    person match {
      case Person("Alice", 25) => println("Hi Alice!")
      case Person("Bob", 32) => println("Hi Bob!")
      case pi(name, age, s) => println("Age: " + age + " year, name: " + name + "?")
      case Person(name, age) => println("Age: " + age + " year, name: " + name + "?")
      case Person(name, 323) =>println("Age:  year, name: " + name + "?")
    }                                             //> Hi Alice!
                                                  //| Hi Bob!
                                                  //| Age: 323 year, name: Charlie?
                                                  //| Age: 32 year, name: Charlie?
                                                  //| Age: 32 year, name: 12?
  }

  // case class, empty one.
  case class Person(name: String, age: Int)
  case class pi(name: Int, age: Int, s: String)

}