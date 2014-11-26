package musica.classic

import musica.math._
import musica.io.HasMidiCode
import musica.io.MidiTunedNote
import musica.io.Midi


trait EitzInterval extends HasMidiCode with MidiTunedNote {
  val basefreq = Midi.baseFrequency(60) // middle C
}

class PureEitzInterval private (p: PureInterval, val note: ClassicNote, val comma: Int) extends PureInterval(p) with EitzInterval {
  
  val midicode = note.midicode 
  val midiFrequency = basefreq * value
  
 def this(note: ClassicNote, comma: Int = 0) = 
     this( ((PureInterval.Fifth * note.fifth) + (PureInterval.SyntonicComma * comma)).normalize
         + PureInterval.Octave * note.octave, note, comma)
    
  override def equals(that: Any): Boolean = {
  that match {
       case that: CentsInterval => this.value == that.value
       case that: PureInterval => this.denom == that.denom && this.numer == that.numer
       case _ => false
     }
   }

    
     
  override def toString = "" + note + "^"+(if(comma>0) "+" + comma else if (comma<0) comma else "0")
}

class RealEitzInterval private (r: RealInterval, val note: ClassicNote, val comma: Rational) extends CentsInterval(r) with EitzInterval {
  val midicode = note.midicode  
  val midiFrequency = basefreq * value
  
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