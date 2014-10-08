package musica.math
import musica.symbol.ClassicNoteParser
import musica.symbol.ClassicNote

class PureInterval(n: Long, d: Long) extends Rational(n: Long, d: Long) with RealInterval {
   
   def this(p: PureInterval) = this(p.numer,p.denom) 
   
   private def pwr(base: Long, to: Int): Long = {
         def _pwr(result: Long, exp: Int): Long = {exp match {
            case 0 => 1 
            case 1 => result
            case _ => _pwr(result*base, exp-1)
        }}
     if (to<=0) 1 else _pwr(base, to)
   }
  
   def +(that: PureInterval) = PureInterval(numer * that.numer, denom * that.denom)
   def -(that: PureInterval) = PureInterval(numer * that.denom, denom * that.numer)
   def *(that: Int) = {
       if (that>0) PureInterval(pwr(numer,that),pwr(denom,that))
       else if (that==0) PureInterval.Prime
       else PureInterval(pwr(denom,-that),pwr(numer,-that))
   } 
   override def unary_- :PureInterval = PureInterval.Prime - this 
 
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
   
    def  on(f: Rational): Rational = {
    this * f
  }
}

object PureInterval {
    def apply(n: Int, d: Int) = new PureInterval(n, d)
    def apply(n: Long, d: Long) = new PureInterval(n, d)
    def apply(i: PureInterval) = new PureInterval(i.numer, i.denom)
    def apply(s: String) =  RealIntervalParser.pure(s)
    implicit def fromString(s: String): PureInterval = PureInterval(s)  
    
    
    val Prime = PureInterval(1, 1)
    val Octave = PureInterval(2, 1)
    val Fifth = PureInterval(3, 2)
    val Fourth = PureInterval(4, 3)
    val MajorThird = PureInterval(5, 4)
    val MinorThird = PureInterval(6, 5)
    val MajorSeventh = PureInterval(7, 6)
    val MinorSeventh = PureInterval(8, 7)
    val MajorTone = PureInterval(9,8)
    val MinorTone = PureInterval(10,9)
    val SemiTone = PureInterval(16,15)
    
    val PythagoreanComma = (Fifth * 12) - (Octave * 7)
    val SyntonicComma = (Fifth * 4) - (MajorThird + (Octave * 2))
    val EnharmonicComma = Octave - (MajorThird * 3)
    
   def JILimit7(f3: Int, f5: Int, f7: Int): PureInterval = {
      (Fifth * f3 + MajorThird * f5 + MajorSeventh * f7).normalize
    }
    
    def JILimit5(f3: Int, f5: Int): PureInterval = {
      (Fifth * f3 + MajorThird * f5).normalize
    }
    
    def JILimit3(f3: Int): PureInterval = {
      (Fifth * f3).normalize
    }

}

class PureEitzInterval(p: PureInterval, val note: ClassicNote, val comma: Int) extends PureInterval(p) {
  def this(note: ClassicNote) = 
     this( ((PureInterval.Fifth * note.fifth) + (PureInterval.SyntonicComma * note.octave)).normalize, note.normalize, note.octave)
    
  def this(note: ClassicNote, comma: Int) = 
     this( ((PureInterval.Fifth * note.fifth) + (PureInterval.SyntonicComma * note.octave)).normalize, note, comma)
    
  override def toString = "" + note + "^"+(if(comma>0) "+" + comma else if (comma<0) comma else "0")
  
}


object EitzInterval { 
  def apply(s: ClassicNote) = new PureEitzInterval(s)
  def apply(s: ClassicNote, l: Int) = new PureEitzInterval(s,l)
  def apply(s: ClassicNote, r: Rational) = new RealEitzInterval(s,r)
  def apply(s: String) =  RealIntervalParser.pure(s)
  
}
