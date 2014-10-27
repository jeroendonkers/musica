package musica.math
import musica.symbol._
import scala.xml.XML

class MappedTuning[N <: SymbolicNote[N,I],I <: SymbolicInterval[I,N]](steplist: List[RealInterval],
    val scale: SymbolicScale[N,I], val base:N, name: String) extends Tuning(steplist,name) {
   
   //def this(steplist: List[RealInterval], seq: NoteSequence) = this(steplist, ClassicScale(seq), 
   //    if (seq.size==0) ClassicNote("C") else seq.notelist(0))
  
   val sequence = scale.on(base)
   def mappedStep(i: Int): (N, RealInterval) = (scale.step(i).on(base), step(i)) 
   
   val centmap = sequence.notelist.zip(centlist)
   val valuemap = sequence.notelist.zip(valuelist)
   
   def intervals(v: ClassicInterval): MappedTuning[N,I] = 
     new MappedTuning(intervals(v.size).steplist, scale,base,"")
     
   def compare(v: ClassicInterval, p: PureInterval): MappedTuning[N,I] = 
     new MappedTuning((intervals(v.size) - Tuning(p,size)).steplist, scale,base,"")
   
   override def -(that: Tuning) = new MappedTuning(steplist.zip(that.steplist).map(e => 
      e match {case (a,b) => a-b }), scale, base,"")  
   
   override def +(that: Tuning) = new MappedTuning(steplist.zip(that.steplist).map(e => 
      e match {case (a,b) => a+b }), scale, base,"")
   
   def frequency(n: Int, refnote: N, reffreq: Double): (N, Double) = {
     val refstep = scale.stepNumber(base.interval(refnote))
     val refint = if (refstep>=0) step(refstep) else RealInterval(0)
     val st = mappedStep(n)
     (st._1,(st._2 - refint).on(reffreq))
   }
   
    def frequencies(n: List[Int], refnote: N, reffreq: Double): List[(N, Double)] = {
     val refstep = scale.stepNumber(base.interval(refnote))
     val refint = if (refstep>=0) step(refstep) else RealInterval(0)
     n.map( e=> mappedStep(e) ). map ( f => f match { case (a,b) => (a, (b-refint).on(reffreq))})
   }
  
    override def save(path: String, name: String, filetag: String, 
               version: String =  "1.0"): Unit = {
     
     XML.save(path+"/"+filetag+".Tuning_Musica_xml",
         
 <musica FileFormat="Tuning" FileFormatVersion="1.00">
  <head>
	<name>{ name }</name>
	<version>{ version }</version>
    <size>{ size }</size>
    <mapped>true</mapped>
  </head>
  <steplist>
   { steplist.zip(sequence.notelist).zipWithIndex.map ( s => 
     <step>
       <index>{ s._2 }</index>
       <note>{ s._1._2.toString }</note>
       <value>{ s._1._1.toString }</value>
     </step> )}
  </steplist>
 </musica>,
                   
   "UTF-8", true, null)   
   }
}




class ClassicMappedTuning(steplist: List[RealInterval],  scale: ClassicScale, base:ClassicNote, name: String="")
extends MappedTuning(steplist,scale,base,name) {
    
    def this(steplist: List[RealInterval], seq: NoteSequence, name: String) = this(steplist, ClassicScale(seq), 
       if (seq.size==0) ClassicNote("C") else seq.notelist(0), name)
  
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


// ASSELIN's notation of temperaments, using circle of fifths

class FifthTuning(val start: Int, val devs: List[Rational], val comma: RealInterval, val name: String="") {
    
            // compute temperated fifths
      private val f = devs.map( a => PureInterval.Fifth + comma * a.value)
       
       // add fifth together in order of Circle 
      private def fsum(f: Int, i: RealInterval, a: List[RealInterval]): List[(ClassicNote, RealInterval)] = {
      a match {
        case List() => List()
        case x :: b => (ClassicNote.FifthCircle(f), (i+x).normalize) :: fsum(f+1, i+x, b)
      }
     }
     private  val g = (ClassicNote.FifthCircle(start), RealInterval(0)) :: fsum(start+1,RealInterval(0),f)
     
     val fifths = g.map(e=> e._1)
     
     // sort the fifths on order of the scale
     private def compare(a: (ClassicNote, RealInterval), b:  (ClassicNote, RealInterval)): Boolean = a._1.chr < b._1.chr
     private val h = g.sortWith(compare)
     
     // subtract interval at C
     val intervals = h.map(e => (e._2-h(0)._2).normalize)
     val notes = NoteSequence(h.map(e => e._1))
     
     // create tuning
     def mappedTuning: ClassicMappedTuning = {    
        new ClassicMappedTuning(intervals, notes, name)
     }
  
      def save(path: String, filetag: String, name: String): Unit = {
     
     XML.save(path+"/"+filetag+".FifthTuning_Musica_xml",
         
 <musica FileFormat="FifthTuning" FileFormatVersion="1.00">
  <head>
	<name>{ name }</name>
    <size>{ devs.size }</size>
    <comma>{ comma.toString }</comma>
    <start>{ ClassicNote.FifthCircle(start) }</start>
  </head>
  <steplist>
   { devs.zip(fifths).zipWithIndex.map ( s => 
     <step>
       <index>{ s._2 }</index>
       <note>{ s._1._2.toString }</note>
       <value>{ s._1._1.toString }</value>
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
