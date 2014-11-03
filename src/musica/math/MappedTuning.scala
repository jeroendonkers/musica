package musica.math
import musica.symbol._
import scala.xml.XML
import scala.collection.immutable.ListMap


class MappedTuning[N <: SymbolicNote[N,I],I <: SymbolicInterval[I,N]](steplist: List[RealInterval],
    val scale: SymbolicScale[N,I], val base:N, name: String) extends Tuning(steplist,name) {
 
   val sequence = scale.on(base)
   def mappedStep(i: Int): (N, RealInterval) = (scale.step(i).on(base), step(i)) 
   
   private def createListMap[T](a: List[(N,T)]) = a.foldLeft(ListMap[N,T]()){
     (m,s) => m + s 
   } 
   
   val stepmap = createListMap(sequence.zip(steplist))
   val centmap = createListMap(sequence.zip(centlist))
   val valuemap = createListMap(sequence.zip(valuelist))
   
   def intervals(v: SymbolicIntervalBase): MappedTuning[N,I] = 
     new MappedTuning(intervals(v.size).steplist, scale,base,"")
     
   def compare(v: SymbolicIntervalBase, p: PureInterval): MappedTuning[N,I] = 
     new MappedTuning((intervals(v.size) - Tuning(p,size)).steplist, scale,base,"")
   
   override def -(that: Tuning) = new MappedTuning(steplist.zip(that.steplist).map(
       {case (a,b) => a-b }), scale, base,"")  
   
   override def +(that: Tuning) = new MappedTuning(steplist.zip(that.steplist).map(
       {case (a,b) => a+b }), scale, base,"")
   
   def frequency(n: Int, refnote: N, reffreq: Double): (N, Double) = {
     val refstep = scale.stepNumber(base.interval(refnote))
     val refint = if (refstep>=0) step(refstep) else RealInterval(0)
     val st = mappedStep(n)
     (st._1,(st._2 - refint).on(reffreq))
   }
   
    def frequencies(n: List[Int], refnote: N, reffreq: Double) = {
     val refstep = scale.stepNumber(base.interval(refnote))
     val refint = if (refstep>=0) step(refstep) else RealInterval(0)
     createListMap(n.map( e=> mappedStep(e) ). map ( { case (a,b) => (a, (b-refint).on(reffreq))}))
   }
  
  
}

