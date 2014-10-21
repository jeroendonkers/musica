package musica.math
import musica.symbol._
import scala.xml.XML

class Tuning(val steplist: List[RealInterval]) {
   def step(i: Int): RealInterval = {
     if (size == 0) {
       RealInterval(0)
     } else {
       val octave = if (i>=0) (i / size) else  ((i+1) / size) - 1  
       val idx = if (i>=0) (i % size) else (size-1- ((-i-1) % size))    
       (PureInterval(2,1) * octave) + steplist(idx)
     }
   }
   val size = steplist.size   
   
   val centlist = steplist.map(i => i.cents)
   val valuelist = steplist.map(i => i.value)
   
   def -(that: Tuning) = Tuning(steplist.zip(that.steplist).map(e => 
      e match {case (a,b) => a-b }))
   def +(that: Tuning) = Tuning(steplist.zip(that.steplist).map(e => 
      e match {case (a,b) => a+b }))
   
      
  def intervals(stepsize: Int) = Tuning(steplist.zipWithIndex.map(e =>
    e match {case (a,i) => step(i+stepsize)-a }
    ))
    
  def mapTo(scale: ClassicScale, base: ClassicNote)  = new MappedTuning(steplist, scale, base)
 
 def save(path: String, name: String, filetag: String, 
               version: String =  "1.0"): Unit = {
     
     XML.save(path+"/"+filetag+".Tuning_Musica_xml",
         
 <musica FileFormat="Tuning" FileFormatVersion="1.00">
  <head>
	<name>{ name }</name>
	<version>{ version }</version>
    <size>{ size }</size>
  </head>
  <steplist>
   { steplist.zipWithIndex.map ( s => 
     <step>
       <index>{ s._2 }</index>
       <value>{ s._1.toString }</value>
     </step> )}
  </steplist>
 </musica>,
                   
   "UTF-8", true, null)   
   }
   
}


object Tuning {
  
  def apply(i: RealInterval, n: Int) = new Tuning(List.range(1,n+1).map(e => i))
  def apply(step: List[RealInterval]) = new Tuning(step)
  def apply(steps: RealInterval*) = new Tuning(steps.toList)
  def apply(st: String) = new Tuning(st.split(",").toList.map(s => RealInterval(s)))  
  
  def ET(n: Int) = Tuning(List.range(1,n+1).map(i => CentsInterval((1200.0 * (i-1)) / n))) 

  val ET12 = ET(12)
  
  def fromRatios(rs: List[PureInterval]): Tuning = {
    def incsum(i: PureInterval, a: List[PureInterval]): List[PureInterval] = {
      a match {
        case List() => List()
        case x :: b => i+x :: incsum(i+x, b)
      }
    }
    new Tuning(PureInterval(1,1) :: incsum(PureInterval(1,1),rs))
  }
  def fromRatios(iis: PureInterval*): Tuning = fromRatios(iis.toList)
  def fromRatios(st: String): Tuning = { 
     fromRatios(st.split(",").toList.map(s => PureInterval(s)))
  }

  def loadXML(filename: String): Tuning = {
    
    val tuningElem = scala.xml.XML.loadFile(filename)
    
    val stepstoomany = ( tuningElem \ "steplist" ).map { steplist => { 
     (steplist \ "step").map (step => (step \ "value").text)
    }}
    val steps = stepstoomany(0).map(s => RealInterval(s)).toList
    new Tuning(steps)
  }
}





class MappedTuning(steplist: List[RealInterval], val scale: ClassicScale, val base: ClassicNote) extends Tuning(steplist) {
   
   def this(steplist: List[RealInterval], seq: NoteSequence) = this(steplist, ClassicScale(seq), 
       if (seq.size==0) ClassicNote("C") else seq.notelist(0))
  
   val sequence = scale.on(base)
   def mappedStep(i: Int): (ClassicNote, RealInterval) = (scale.step(i).on(base), step(i)) 
   
   val centmap = sequence.notelist.zip(centlist)
   val valuemap = sequence.notelist.zip(valuelist)
   
   def intervals(v: ClassicInterval): MappedTuning = 
     new MappedTuning(intervals(v.size).steplist, scale,base)
     
   def compare(v: ClassicInterval, p: PureInterval): MappedTuning = 
     new MappedTuning((intervals(v.size) - Tuning(p,size)).steplist, scale,base)
   
   override def -(that: Tuning) = new MappedTuning(steplist.zip(that.steplist).map(e => 
      e match {case (a,b) => a-b }), scale, base)  
   
   override def +(that: Tuning) = new MappedTuning(steplist.zip(that.steplist).map(e => 
      e match {case (a,b) => a+b }), scale, base)
   
   def frequency(n: Int, refnote: ClassicNote, reffreq: Double): (ClassicNote, Double) = {
     val refstep = scale.stepNumber(ClassicInterval(base, refnote))
     val refint = if (refstep>=0) step(refstep) else RealInterval(0)
     val st = mappedStep(n)
     (st._1,(st._2 - refint).on(reffreq))
   }
   
    def frequencies(n: List[Int], refnote: ClassicNote, reffreq: Double): List[(ClassicNote, Double)] = {
     val refstep = scale.stepNumber(ClassicInterval(base, refnote))
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

class FifthTuning(val start: Int, val devs: List[Rational], val comma: RealInterval) {
    
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
  def mappedTuning: MappedTuning = {    
      new MappedTuning(intervals, notes)
    
  }
  
      def save(path: String, name: String, filetag: String, 
               version: String =  "1.0"): Unit = {
     
     XML.save(path+"/"+filetag+".FifthTuning_Musica_xml",
         
 <musica FileFormat="FifthTuning" FileFormatVersion="1.00">
  <head>
	<name>{ name }</name>
	<version>{ version }</version>
    <size>{ devs.size }</size>
    <comma>{ comma.toString }</comma>
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
  def apply(start: Int, devs: List[Rational], comma: RealInterval) = new
      FifthTuning(start, devs, comma)
 
  def apply(st: String) = {  
    val terms = st.split(",").toList
    if (terms.size < 2) {
      new FifthTuning(0,List(), PureInterval.SyntonicComma)
    } else {
       val note: ClassicNote = terms(0)
       val comma = if (terms(1)=="S") PureInterval.SyntonicComma else PureInterval.PythagoreanComma
       val rations = terms.tail.tail.map(s => RealIntervalParser.aratio(s))   
       new FifthTuning(note.fifth,rations, comma)
    }
  }
  
  
}

