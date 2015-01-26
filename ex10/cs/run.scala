package ex10.cs

object run {

  def main(args: Array[String]): Unit = {

    if (args.length > 0) {

      //get Dir from args
      val Fullpath = args(0)
      // read all files in dir to list
      val files = helpers.fileList(Fullpath)

      val path = Fullpath.split("/").last
      
      val tok = new tokenizing
        
      for (file <- files) {
       
    	 def d = helpers.writeTranslatToFile(file+"X",".xml") _ 
    	 
    	 tok.WriteXmlTokens(
    	     tok.parsToTokens(
    	         helpers.allFileLines(
    	             file))
    	             ,d)
      }
      // val  fileName = "StackTest.vm"

    }
  }
}