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
 

   
}


case class HauptwerkSpecs(name: String, 
                          shortname: String, 
                          filetag: String, 
                          id: String,
                          version: String =  "1.0",
                          refnote: ClassicNote = ClassicNote("A"), 
                          reffreq: Double = 440.0)   

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
  
    def exportHauptwerk(path: String, specs: HauptwerkSpecs): Unit = {
     
     XML.save(path+"/"+specs.filetag+".Temperament_Hauptwerk_xml",
         
 <Hauptwerk FileFormat="Temperament" FileFormatVersion="4.00">
  <ObjectList ObjectType="_General">
  <_General>
	<Sys_ObjectID>1</Sys_ObjectID>
	<UniqueTemperamentID>{ specs.id }</UniqueTemperamentID>
	<Name>{ specs.name }</Name>
	<ShortName>{ specs.shortname }</ShortName>
	<SupplierID>888888</SupplierID>
	<SupplierName>Musica Scala Library</SupplierName>
	<Comments></Comments>
	<TemperamentVersion>{ specs.version }</TemperamentVersion>
  </_General>
  </ObjectList>
  <ObjectList ObjectType="note">
   { frequencies(List.range(-60,73), specs.refnote, specs.reffreq). 
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

  
  // ASSELIN's notation of temperaments, using circle of fifths
  def fromFifths(start: Int, devs: List[Rational], comma: RealInterval): MappedTuning = {
    
       // compute temperated fifths
       val f = devs.map( a => PureInterval.Fifth + comma * a.value)
       
       // add fifth together in order of Circle 
      def fsum(f: Int, i: RealInterval, a: List[RealInterval]): List[(ClassicNote, RealInterval)] = {
      a match {
        case List() => List()
        case x :: b => (ClassicNote.FifthCircle(f), (i+x).normalize) :: fsum(f+1, i+x, b)
      }
     }
     val g = (ClassicNote.FifthCircle(start), RealInterval(0)) :: fsum(start+1,RealInterval(0),f)
     
     // sort the fifths on order of the scale
     def compare(a: (ClassicNote, RealInterval), b:  (ClassicNote, RealInterval)): Boolean = a._1.chr < b._1.chr
     val h = g.sortWith(compare)
     
     // subtract interval at C
     val intervals = h.map(e => (e._2-h(0)._2).normalize)
     val notes = NoteSequence(h.map(e => e._1))
     
     // create tuning
      new MappedTuning(intervals, notes)
   }
  
  def fromFifths(st: String): MappedTuning = {  
    val terms = st.split(",").toList
    if (terms.size < 2) {
      new MappedTuning(List(), ClassicScale(List()), "C")
    } else {
       val note: ClassicNote = terms(0)
       val comma = if (terms(1)=="S") PureInterval.SyntonicComma else PureInterval.PythagoreanComma
       val rations = terms.tail.tail.map(s => RealIntervalParser.aratio(s))   
       fromFifths(note.fifth,rations, comma)
    }
  }
  
 
}