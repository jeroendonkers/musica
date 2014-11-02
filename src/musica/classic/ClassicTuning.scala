package musica.classic
import musica.symbol._
import musica.math._
import scala.xml.XML


class ClassicMappedTuning(steplist: List[RealInterval],  scale: ClassicScale, base:ClassicNote, name: String="")
extends MappedTuning(steplist,scale,base,name) {
    
    def this(steplist: List[RealInterval], seq: NoteSequence, name: String) = this(steplist, ClassicScale(seq), 
       if (seq.size==0) ClassicNote("C") else seq.notelist(0), name)
       
    // auxillary constructor for fifth tuning   
    def this(info: List[(ClassicNote, RealInterval)], name: String) = this(info.unzip._2, new NoteSequence(info.unzip._1), name)   
       
   override def saveXML(path: String, filetag: String, name: String,  
               version: String =  "1.0"): Unit = {
     
     XML.save(path+"/"+filetag+".MappedTuning_Musica_xml",
         
 <musica FileFormat="MappedTuning" FileFormatVersion="1.00">
  <head>
	<name>{ name }</name>
	<version>{ version }</version>
    <size>{ size }</size>
  </head>
  <steplist>
   { steplist.zip(sequence.notelist).map ( s => 
     <step>
        <note>{ s._2.toString }</note>
       <value>{ s._1.toString }</value>
     </step> )}
  </steplist>
 </musica>,
                   
   "UTF-8", true, null)   
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
   
     def loadXML(filename: String): Either[ClassicMappedTuning, String] = {
    
    try {
      val tuningElem = scala.xml.XML.loadFile(filename)
          
      if ((tuningElem \  "@FileFormat").text != "MappedTuning") {
        throw new Exception("Wrong file type \nexpecting MappedTuning_Musica_xml.")  
      } 
      
      val name = ((tuningElem \ "head") \ "name"). text
      val size = ((tuningElem \ "head") \ "size"). text. toInt
 
      val stepstoomany = ( tuningElem \ "steplist" ).map { steplist => { 
       (steplist \ "step").map (step => (step \ "value").text)
      }}
    
      if (stepstoomany(0).size != size) {
         throw new Exception("Error in file: number of steps not equal to size.") 
      }   
      
      val notenames  = ( tuningElem \ "steplist" ).map { steplist => { 
       (steplist \ "step").map (step => (step \ "note").text)
      }}
      
      val steps = stepstoomany(0).mkString(",")
      val notes = notenames(0).mkString(",")
    
      Left(ClassicMappedTuning(steps,notes,name))
    } catch {
      case e: Exception => Right(e.getMessage)
    } 
  }
}


// ASSELIN's notation of temperaments, using circle of fifths

class FifthTuning(val start: Int, val devs: List[Rational], val comma: RealInterval, name: String="") 
  extends ClassicMappedTuning(FifthTuning.createMap(start,devs,comma), name){
      
      override def saveXML(path: String, filetag: String, name: String, 
               version: String =  "1.0"): Unit = {
     
     XML.save(path+"/"+filetag+".FifthTuning_Musica_xml",
         
 <musica FileFormat="FifthTuning" FileFormatVersion="1.00">
  <head>
	<name>{ name }</name>
    <size>{ devs.size }</size>
    <comma>{ comma.toString }</comma>
    <start>{ ClassicNote.FifthCircle(start) }</start>
  </head>
  <steplist>
   { devs.map ( s => 
     <step>
       <value>{ s.toString }</value>
     </step> )}
  </steplist>
 </musica>,
                   
   "UTF-8", true, null)   
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
  
  
   def loadXML(filename: String): Either[FifthTuning, String] = {
    
    try {
      val tuningElem = scala.xml.XML.loadFile(filename)
          
      if ((tuningElem \  "@FileFormat").text != "FifthTuning") {
        throw new Exception("Wrong file type \nexpecting FifthTuning_Musica_xml.")  
      } 
      
      val start = ((tuningElem \ "head") \ "start"). text 
      val comma = ((tuningElem \ "head") \ "comma"). text      
      val name = ((tuningElem \ "head") \ "name"). text
 
      val stepstoomany = ( tuningElem \ "steplist" ).map { steplist => { 
       (steplist \ "step").map (step => (step \ "value").text)
      }}
    
      if (stepstoomany(0).size != 11) {
         throw new Exception("Error in file: expected 11 steps.") 
      }   
      
      val steps = stepstoomany(0).mkString(",")
    
      Left(FifthTuning(start+","+comma+","+steps,name))
    } catch {
      case e: Exception => Right(e.getMessage)
    } 
  }
}