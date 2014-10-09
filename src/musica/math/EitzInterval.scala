package musica.math
import musica.symbol.ClassicNote

class PureEitzInterval(p: PureInterval, val note: ClassicNote, val comma: Int) extends PureInterval(p) {
  def this(note: ClassicNote) = 
     this( ((PureInterval.Fifth * note.fifth) + (PureInterval.SyntonicComma * note.octave)).normalize, note.normalize, note.octave)
    
  def this(note: ClassicNote, comma: Int) = 
     this( ((PureInterval.Fifth * note.fifth) + (PureInterval.SyntonicComma * comma)).normalize, note, comma)
    
  override def equals(that: Any): Boolean = {
  that match {
       case that: CentsInterval => this.value == that.value
       case that: PureInterval => this.denom == that.denom && this.numer == that.numer
       case _ => false
     }
   }

    
     
  override def toString = "" + note + "^"+(if(comma>0) "+" + comma else if (comma<0) comma else "0")
}

class RealEitzInterval(r: RealInterval, val note: ClassicNote, val comma: Rational) extends CentsInterval(r) {
    
  def this(note: ClassicNote, comma: Rational) = 
    this( ((PureInterval.Fifth * note.fifth) +
         (PureInterval.SyntonicComma * comma.value)).normalize  
         + CentsInterval(1200) * note.octave,  note,  comma) 
       
  override def toString = "" + note + "^"+(if(comma.numer>0) "+" + comma else if (comma.numer<0) comma else "0")
}



object EitzInterval { 
  def apply(s: ClassicNote) = new PureEitzInterval(s)
  def apply(s: ClassicNote, l: Int) = new PureEitzInterval(s,l)
  def apply(s: ClassicNote, r: Rational) = new RealEitzInterval(s,r)
  def apply(s: ClassicNote, n: Int, m: Int) = new RealEitzInterval(s,Rational(n,m))
  def apply(s: String) =  RealIntervalParser.eitz(s)
  def pure(s: String) =  RealIntervalParser.pureEitz(s)
}