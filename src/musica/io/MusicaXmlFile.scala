package musica.io
import scala.xml

case class MusicaXmlData(val header: Map[String,String], val data: Map[String,List[String]])


// helper class to store and load XML data
// general format: a header section and a data section containing a list of data records
// filetype: unique name of this Musica file type
class MusicaXmlFile(val filetype: String, val version: String, val datablocktag: String, val datatag: String) {
 
  // save data present in xmldata to specified path and filetag (no extensoion)
  
  def saveXML(path: String, filetag: String, xmldata: MusicaXmlData): Unit = { 
    
  def createXMLdata(data: Map[String,List[String]]): String = {  
   def peel(m: Map[String,List[String]] ): Map[String,List[String]]  = {
     m.map {case (a,l) => { (a,l.tail)}} . filter((t) => t._2.size>0)
   }
   def fetchFront(m: Map[String,List[String]] ) = {
     m.map {case (a,l) => { "<"+a+">"+ l.head +"</"+a+">"}} .mkString("")
   }
   def toXMLs(m: Map[String,List[String]], tag: String ): String = {
      if (m.size==0) "" else "<"+tag+">"+fetchFront(m) + "</"+tag+">\n"+toXMLs(peel(m), tag)
   }   
   "<"+datablocktag+">\n" + toXMLs(data.filter((t) => t._2.size>0),datatag) + "</"+datablocktag+">\n"
  }
  
  def createXMLhead(header: Map[String,String]): String = {
      "<head>" +  header.map {case (a,l) => { "<"+a+">"+ l +"</"+a+">"}} .mkString("") + "</head>\n"
   }
  

   
     xml.XML.save(path+"/"+filetag+"."+ filetype+"_Musica_xml",
<musica FileFormat={ filetype } FileFormatVersion= { version }> 
{ xml.XML.loadString ( createXMLhead(xmldata.header))  }
{ xml.XML.loadString ( createXMLdata(xmldata.data) ) }
</musica>
     ,
                   
   "UTF-8", true, null)   
   }
  
  
   // Load data from XML file into a MusicaXmlData object
   // In case of an error, a string containing the error message is returned Right
   // only keys in parameter expect are used as model for data to be retrieved
   def loadXML(filename: String, expect: MusicaXmlData): Either[MusicaXmlData, String] = {
    try {
        
      val tuningElem = scala.xml.XML.loadFile(filename)
          
      if ((tuningElem \  "@FileFormat").text != filetype) {
        throw new Exception("Wrong file type \nexpecting "+filetype+"_Musica_xml.")  
      } 
      val header = expect.header.map({ case (a,_) => (a,((tuningElem \ "head") \ a). text)})
      
      def getdata(a: String) = {
        val toomany =   (tuningElem \ datablocktag ). map ( l =>  (l \ datatag). map( v => (v \ a).text))
        toomany(0).toList
      }
      val data = expect.data.map({ case (a,_) => (a, getdata(a))})
      
     Left(new MusicaXmlData(header,data))
    } catch {
      case e: Exception => Right(e.getMessage)
    } 
    }
  
}