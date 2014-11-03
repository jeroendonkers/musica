package musica.io

object MusicaXmlFileEx {
  def main(args: Array[String]): Unit = {
     
    val f = new MusicaXmlFile("Tuning","1.0","steplist","step")
    
    val header = Map("name"-> "Een naam")
    val data = Map("value"-> List("2","1","3"),
                   "note" -> List("C","D"),
                   "empty" -> List())
    
    f.saveXML("./data","test", MusicaXmlData( header, data))
    
    
    f.loadXML("./data/test.Tuning_Musica_xml",  MusicaXmlData( header, data)) match {
        case Right(s) => println("Error "+ s)
        case Left(u) =>  println("Succes "+u)
      
    }
    
    
  }
}