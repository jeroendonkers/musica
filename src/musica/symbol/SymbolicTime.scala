package musica.symbol
import musica.math.Rational

class NoteSize(n: Long, m: Long) extends Rational(n,m) {
  
  def this(r: Rational) = this(r.numer, r.denom)
  
  def *(that: Int): NoteSize = { new NoteSize(n*that,m) }
  def /(that: Int): NoteSize = { new NoteSize(n,m*that) }
   
  lazy val dot: NoteSize = new NoteSize(3*n, 2*m) 
  lazy val dotdot: NoteSize =  new NoteSize(7*n, 4*m) 
  lazy val triad: NoteSize =  new NoteSize(n,m*3) 
}

class Metrum(val base: NoteSize, val count: Int, val name: String) {
  override def toString(): String = name+": "+count + "x"+base
}

object Metrum {
  def apply(n: Int, m: Int) =  
    new Metrum( new NoteSize(1,m), n, ""+ n + "/" + m)
}

trait HasNoteSize {
   val notesize: NoteSize
}

object HasNoteSize { 
   def add(notes: List[HasNoteSize]) = 
     new NoteSize((Rational(0,1) :: notes.map(ns => ns.notesize.asInstanceOf[Rational])) 
                          reduceLeft {(x,y) => x+y})

}

class Rest(val notesize: NoteSize) extends HasNoteSize
