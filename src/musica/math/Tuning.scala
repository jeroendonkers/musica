package musica.math
import musica.symbol._

class Tuning(val steplist: List[RealInterval]) {
   def step(i: Int): RealInterval = {
     val octave = if (i>=0) (i / size) else  ((i+1) / size) - 1  
     val idx = if (i>=0) (i % size) else (size-1- ((-i-1) % size))    
     (PureInterval(2,1) * octave) + steplist(idx)
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


class MappedTuning(steplist: List[RealInterval], val scale: ClassicScale, val base: ClassicNote) extends Tuning(steplist) {
   
   val notelist = scale.on(base)
   def mappedStep(i: Int): (ClassicNote, RealInterval) = (scale.step(i).on(base), step(i)) 
   
   val centmap = notelist.zip(centlist)
   val valuemap = notelist.zip(valuelist)
   
   def intervals(v: ClassicInterval): List[(ClassicNote, RealInterval)] = 
     notelist.zip(intervals(v.size).steplist)
     
}



object Tuning {
  
  def apply(i: RealInterval, n: Int) = new Tuning(List.range(1,n+1).map(e => i))
  def apply(step: List[RealInterval]) = new Tuning(step)
  def apply(steps: RealInterval*) = new Tuning(steps.toList)
  def apply(st: String) = new Tuning(st.split(",").toList.map(s => RealInterval(s)))  
  
 
  
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
  def fromFifths(start: Int, devs: List[Rational], comma: RealInterval): Tuning = {
    
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
     val h = g.sortWith(compare).map(e => e._2)
     
     // subtract interval at C
     val i = h.map(e => (e-h(0)).normalize)
    
     // create tuning
      new Tuning(i)
   }
  
  def fromFifths(st: String,  comma: RealInterval = PureInterval.SyntonicComma): Tuning = {  
    val terms = st.split(",").toList
    val note: ClassicNote = terms(0)
    val rations = terms.tail.map(s => RealIntervalParser.aratio(s))   
    fromFifths(note.fifth,rations, comma)
  }
  
  def ET(n: Int) = Tuning(List.range(1,n+1).map(i => CentsInterval((1200.0 * (i-1)) / n))) 

  val ET12 = ET(12)
}