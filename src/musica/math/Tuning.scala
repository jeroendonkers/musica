package musica.math

class Tuning(val steplist: List[RealInterval]) {
   def step(i: Int): RealInterval = {
     val octave = if (i>=0) (i / size) else  ((i+1) / size) - 1  
     val idx = if (i>=0) (i % size) else (size-1- ((-i-1) % size))    
     (PureInterval(2,1) * octave) + steplist(idx)
   }
   val size = steplist.size   
   
}

object Tuning {
  def apply(step: List[RealInterval]) = new Tuning(step)
  def apply(steps: RealInterval*) = new Tuning(steps.toList)
  def apply(st: String) = new Tuning(st.split(",").toList.map(s => RealInterval(s)))  
  
  def fromRatios(st: String) = {
    val rs = st.split(",").toList.map(s => RealInterval(s))
    
  }
  
}