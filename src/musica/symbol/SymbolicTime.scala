package musica.symbol
import musica.math.Rational

class NoteValue(n: Long, m: Long) extends Rational(n,m) {
  
  def this(r: Rational) = this(r.numer, r.denom)
  
  def *(that: Int): NoteValue = { new NoteValue(n*that, m) }
  def /(that: Int): NoteValue = { new NoteValue(n, m*that) }
   
  lazy val dot: NoteValue = new NoteValue(3*n, 2*m) 
  lazy val dotdot: NoteValue =  new NoteValue(7*n, 4*m) 
  lazy val triad: NoteValue =  new NoteValue(n, m*3) 
}


class Metrum(val base: NoteValue, val count: Int, val name: String) {
  override def toString(): String = name+": "+count + "x"+base
}

object Metrum {
  def apply(n: Int, m: Int) =  
    new Metrum( new NoteValue(1,m), n, ""+ n + "/" + m)
}

trait TimeSeg {
   val notevalue: NoteValue
}

class Rest(val notevalue: NoteValue) extends TimeSeg

class SymbolicVNote[T <: SymbolicNoteBase](val note: T, val notevalue: NoteValue) extends TimeSeg

class TimeSegList(val segs: List[TimeSeg]) extends TimeSeg {
  val notevalue = new NoteValue(segs.map(e => e.notevalue.asInstanceOf[Rational]).sum)  
}