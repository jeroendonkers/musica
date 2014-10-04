package musica.math

trait RealInterval {
  protected val ln2 = Math.log(2)
  val value: Double
  lazy val cents = 1200 * Math.log(value) / ln2
  
  override def equals(that: Any): Boolean = {
  that match {
       case that: RealInterval => this.value == that.value
       case _ => false
     }
   }

  
  
  def +(that: RealInterval) = CentsInterval(cents + that.cents) 
  def -(that: RealInterval) = CentsInterval(cents - that.cents)
  def *(that: Double) = CentsInterval(cents * that)
  def /(that: Double) = CentsInterval(cents / that)
  def unary_- :RealInterval = CentsInterval(-cents)
  
  // bring back to range [0 - 1200] 
  def normalize(): RealInterval = {
    CentsInterval(cents - (cents/1200.0).floor * 1200.0 )    
  }
  
  // apply interval to frequency
  def  on(f: Double): Double = {
    f * value
  }
}

class CentsInterval(c: Double) extends RealInterval {
  lazy override val cents = c
  lazy val value = Math.pow(2,c/1200.0)
  override def toString = "" + cents + " c"
  override def unary_- :CentsInterval = CentsInterval(-cents)
} 

object CentsInterval {
  def apply(d: Double) = new CentsInterval(d)
}

object RealInterval {
  def apply(d: Double) = CentsInterval(d)
  implicit def fromDouble(d: Double): RealInterval = CentsInterval(d) 
  implicit def toDouble(r: RealInterval): Double = r.cents
  def apply(n: Int, m: Int) = PureInterval(n,m)
  def Octave = PureInterval.Octave
  def Prime = PureInterval.Prime
}
