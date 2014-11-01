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


class NoteSystem(val perfect: Boolean, val complete: Boolean) {
  val Whole = new NoteSize(1,1)
  val Lunga = Whole * (if (perfect) 3 else 2)
  val White = Whole / (if (complete) 3 else 2)
  val Quarter = White / 2
  val Eighth = Quarter / 2
  val Sixteenth = Eighth / 2
  val ThirtySecondth = Sixteenth / 2
  val SixtyFourth = ThirtySecondth / 2
}

object ClassicNoteSystem extends NoteSystem(false, false) 

class Metrum(val base: NoteSize, val count: Int, val name: String) {
  override def toString(): String = name+": "+count + "x"+base
}

object Metrum {
  def apply(n: Int, m: Int) =  
    new Metrum( new NoteSize(1,m), n, ""+ n + "/" + m)
}


object ClassicMetrum {
  def apply(n: Int, m: Int) =  
    new Metrum( ClassicNoteSystem.Whole/m, n, ""+ n + "/" + m)

  val C = new Metrum(ClassicNoteSystem.Quarter,4,"C")
  val AllaBreva = new Metrum(ClassicNoteSystem.Quarter,2,"C|")
       
}

trait HasNoteSize {
   val notesize: NoteSize
}

class Rest(val notesize: NoteSize) extends HasNoteSize

object ClassicRest {
   val Whole = new Rest(ClassicNoteSystem.Whole)
   val Half = new Rest(ClassicNoteSystem.White)
   val Quarter = new Rest(ClassicNoteSystem.Quarter)
}


object HasNoteSize {
 
   def add(notes: List[HasNoteSize]) = 
     new NoteSize((Rational(0,1) :: notes.map(ns => ns.notesize.asInstanceOf[Rational])) 
                          reduceLeft {(x,y) => x+y})

   
}