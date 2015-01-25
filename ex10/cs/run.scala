package ex10.cs

object run {

  def main(args: Array[String]): Unit = {
    //get file in a 
		  val data = helpers.allFileLines("C:/tmp/p10/ArrayTest/Main.jack").replaceAll("""(?:\/\*\*.+\*\/)""", "").split("\\s").filterNot(_ =="").toList
		  val tok = new tokenizing
		  var s = """Keyboard.readInt("HOW MANY NUMBERS? ");"""
		   println(tok.parsToTokens(s))
		  
  }
  
}