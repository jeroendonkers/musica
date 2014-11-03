package musica.classic
import musica.symbol._
import musica.math._
import musica.io._
import scala.xml.XML


class ClassicMappedTuning(steplist: List[RealInterval],  scale: ClassicScale, base:ClassicNote, name: String="")
extends MappedTuning(steplist,scale,base,name) {
    
    def this(steplist: List[RealInterval], seq: NoteSequence, name: String) = this(steplist, ClassicScale(seq), 
       if (seq.size==0) ClassicNote("C") else seq.notelist(0), name)
       
    // auxillary constructor for fifth tuning   
    def this(info: List[(ClassicNote, RealInterval)], name: String) = this(info.unzip._2, new NoteSequence(info.unzip._1), name)   
       
   override def saveXML(path: String, filetag: String, name: String,  
               version: String =  "1.0"): Unit = {
         ClassicMappedTuning.xmlfile.saveXML(path, filetag, new MusicaXmlData(
        Map("name" -> name, "version" -> version, "size"-> size.toString),
        Map("value" -> steplist.map(s => s.toString), 
            "note"-> sequence.notelist.map(s => s.toString))   
     ))
   } 
    
    
    def exportHauptwerk(path: String,
                          name: String, 
                          shortname: String, 
                          filetag: String, 
                          id: String,
                          version: String =  "1.0",
                          refnote: ClassicNote = ClassicNote("A"), 
                          reffreq: Double = 440.0): Unit = {
     
     XML.save(path+"/"+filetag+".Temperament_Hauptwerk_xml",
         
 <Hauptwerk FileFormat="Temperament" FileFormatVersion="4.00">
  <ObjectList ObjectType="_General">
  <_General>
	<Sys_ObjectID>1</Sys_ObjectID>
	<UniqueTemperamentID>{ id }</UniqueTemperamentID>
	<Name>{ name }</Name>
	<ShortName>{ shortname }</ShortName>
	<SupplierID>888888</SupplierID>
	<SupplierName>Musica Scala Library</SupplierName>
	<Comments></Comments>
	<TemperamentVersion>{ version }</TemperamentVersion>
  </_General>
  </ObjectList>
  <ObjectList ObjectType="note">
   { frequencies(List.range(-60,73), refnote, reffreq). 
     map(e => { e match { case (a,b) => {
  <note>
     <MIDINoteNumberOnEightFootStop>{ a.midicode }</MIDINoteNumberOnEightFootStop>
     <PitchHz>{ b }</PitchHz>
  </note>
		    }}})}
  </ObjectList>
 </Hauptwerk>,
                     
   "UTF-8", true, null)   
   }
}

object ClassicMappedTuning {
  
   def apply(st: String, nts: String, name: String) = new ClassicMappedTuning(
       st.split(",").toList.map(RealInterval(_)),
       new NoteSequence(nts.split(",").toList.map(ClassicNote(_))),
       name)    
   
   val xmlfile = new MusicaXmlFile("ClassicMappedTuning", "1.0", "steplist", "step") 
   
   def loadXML(filename: String): Either[ClassicMappedTuning, String] = {   
        xmlfile.loadXML(filename, new MusicaXmlData(
        Map("name" ->"" , "version" -> "", "size"-> ""),
        Map("value" -> List(), "note"-> List()))) match {
        case Left(m) => {
           val name = m.header("name")
           val size = m.header("size").toInt
           val steps = m.data("value")
           val notes = m.data("note")
           if (steps.size != size) Right("Error in file: number of steps not equal to size.")
           Left(ClassicMappedTuning(steps.mkString(","),notes.mkString(","),name))
       }
       case Right(s) => Right(s)
     }
   }
}


// ASSELIN's notation of temperaments, using circle of fifths

class FifthTuning(val start: Int, val devs: List[Rational], val comma: RealInterval, name: String="") 
  extends ClassicMappedTuning(FifthTuning.createMap(start,devs,comma), name){
      
      override def saveXML(path: String, filetag: String, name: String, 
               version: String =  "1.0"): Unit = {
         FifthTuning.xmlfile.saveXML(path, filetag, new MusicaXmlData(
        Map("name" -> name, "version" -> version,"size"-> devs.size.toString, 
            "comma" -> { if (comma == PureInterval.SyntonicComma) "S"else "P"} , 
            "start"-> ClassicNote.FifthCircle(start).toString),
        Map("value" -> devs.map(s => s.toString) ) ) )       
      }
}


object FifthTuning {
  def apply(start: Int, devs: List[Rational], comma: RealInterval,name: String) = new
      FifthTuning(start, devs, comma,name)
 
  def apply(st: String, name: String = "") = {  
    val terms = st.split(",").toList
    if (terms.size < 2) {
      new FifthTuning(0,List(), PureInterval.SyntonicComma)
    } else {
       val note: ClassicNote = terms(0)
       val comma = if (terms(1)=="S") PureInterval.SyntonicComma else PureInterval.PythagoreanComma
       val rations = terms.tail.tail.map(s => RealIntervalParser.aratio(s))   
       new FifthTuning(note.fifth,rations, comma, name)
    }
  }
  
     // create an ordered list of intervals and note names to feed in the constructor of mapped tuning.
     def createMap(start: Int, devs: List[Rational], comma: RealInterval): List[(ClassicNote, RealInterval)] =  {
         val f = devs.map( a => PureInterval.Fifth + comma * a.value)
       
       // add fifth together in order of Circle 
         def fsum(f: Int, i: RealInterval, a: List[RealInterval]): List[(ClassicNote, RealInterval)] = {
         a match {
           case List() => List()
           case x :: b => (ClassicNote.FifthCircle(f), (i+x).normalize) :: fsum(f+1, i+x, b)
         }
          }
        val g = (ClassicNote.FifthCircle(start), RealInterval(0)) :: fsum(start+1,RealInterval(0),f)    
        val fifths = g.map(e=> e._1)
     
       // sort the fifths on order of the scale
       def compare(a: (ClassicNote, RealInterval), b:  (ClassicNote, RealInterval)): Boolean = a._1.chr < b._1.chr
       val h = g.sortWith(compare)
        // subtract interval at C
       h.map(e => (e._1, (e._2-h(0)._2).normalize))     
     }
  
   val xmlfile = new MusicaXmlFile("FifthTuning", "1.0", "steplist", "step") 
  
   def loadXML(filename: String): Either[FifthTuning, String] = {
    
        xmlfile.loadXML(filename, new MusicaXmlData(
        Map("name" ->"" , "version" -> "", "size"-> "", "start" -> "", "comma"-> ""),
        Map("value" -> List()))) match {
        case Left(m) => {
           val name = m.header("name")
           val size = m.header("size").toInt
           val comma = m.header("comma")
           val start = m.header("start")
           val steps = m.data("value")
           if (steps.size != size) Right("Error in file: number of steps not equal to size.")
           Left(FifthTuning(start+","+comma+","+steps.mkString(","),name))
       }
       case Right(s) => Right(s)
     } 
  }
}