package musica.math

class PureInterval(n: Long, d: Long) extends Rational(n: Long, d: Long) with RealInterval {
   
   private def pwr(base: Long, to: Int): Long = {
         def _pwr(result: Long, exp: Int): Long = {exp match {
            case 0 => 1 
            case 1 => result
            case _ => _pwr(result*base, exp-1)
        }}
     if (to<=0) 1 else _pwr(base, to)
   }
  
   def +(that: PureInterval) = new PureInterval(numer * that.numer, denom * that.denom)
   def -(that: PureInterval) = new PureInterval(numer * that.denom, denom * that.numer)
   def *(that: Int) = {
       if (that>0) new PureInterval(pwr(numer,that),pwr(denom,that))
       else PureInterval.Prime
   } 
   override def unary_- :PureInterval = PureInterval.Octave - this 
 
   // normalize to rational between 1/1 and 2/1
   override def normalize(): PureInterval = {
     if (value>2) this - PureInterval.Octave * ( (Math.log(value)/ln2).floor.toInt)
     else if (value<1) this + PureInterval.Octave * (-(Math.log(value)/ln2).floor.toInt)
     else this
   }
   
   // apply interval on whole number frequency: return rational
   def  on(f: Int): Rational = {
    this * Rational(f,1)
  }
}

object PureInterval {
    def apply(n: Int, d: Int) = new PureInterval(n, d)
    def apply(n: Long, d: Long) = new PureInterval(n, d)
    val Prime = new PureInterval(1, 1)
    val Octave = new PureInterval(2, 1)
    val Fifth = new PureInterval(3, 2)
    val Fourth = new PureInterval(4, 3)
    val MajorThird = new PureInterval(5, 4)
    val MinorThird = new PureInterval(6, 5)
    val MajorSeventh = new PureInterval(7, 6)
    val MinorSeventh = new PureInterval(8, 7)
    val MajorTone = new PureInterval(9,8)
    val MinorTone = new PureInterval(10,9)
    val SemiTone = new PureInterval(16,15)
    
    val PythagoreanComma = (Fifth * 12) - (Octave * 7)
    val SyntonicComma = (Fifth * 4) - (MajorThird + (Octave * 2))
    val EnharmonicComma = Octave - (MajorThird * 3)

   def JitLimit7(f3: Int, f5: Int, f7: Int): PureInterval = {
      (Fifth * f3 + MajorThird * f5 + MajorSeventh * f7).normalize
    }
    
    def JitLimit5(f3: Int, f5: Int): PureInterval = {
      (Fifth * f3 + MajorThird * f5).normalize
    }
    
    def JitLimit3(f3: Int): PureInterval = {
      (Fifth * f3).normalize
    }
      
}

