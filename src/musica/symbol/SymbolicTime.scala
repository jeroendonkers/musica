package musica.symbol
import musica.math.Rational
import scala.collection.immutable.ListMap

class NoteValue(n: Long, m: Long) extends Rational(n,m) {
  
  def this(r: Rational) = this(r.numer, r.denom)
   
   def +(that: NoteValue): NoteValue = new NoteValue(super.+(that))
   def -(that: NoteValue): NoteValue = new NoteValue(super.-(that))
   def *(that: NoteValue): NoteValue = new NoteValue(super.*(that))
   def /(that: NoteValue): NoteValue = new NoteValue(super./(that))
   override def unary_- :NoteValue = new NoteValue(super.unary_-)
  
  def *(that: Int): NoteValue = { new NoteValue(n*that, m) }
  def /(that: Int): NoteValue = { new NoteValue(n, m*that) }
   
  lazy val dot: NoteValue = new NoteValue(3*n, 2*m) 
  lazy val dotdot: NoteValue =  new NoteValue(7*n, 4*m) 
  lazy val triad: NoteValue =  new NoteValue(n, m*3) 
}

object NoteValue {
    def apply(n: Long, d: Long) = new NoteValue(n,d)  
    def apply(n: Int, d: Int) = new NoteValue(n,d) 
  
  implicit object NoteValueNumeric extends Numeric[NoteValue]  {
   // implementing Numeric trait
    def compare(x: NoteValue, y: NoteValue): Int = {
      if (x.value < y.value) -1 else +1
    }
    def fromInt(x: Int): NoteValue = NoteValue(x,1)
    def minus(x: NoteValue, y: NoteValue): NoteValue = x - y
    def negate(x: NoteValue): NoteValue = -x
    def plus(x: NoteValue, y: NoteValue): NoteValue = x + y
    def times(x: NoteValue, y: NoteValue): NoteValue = x*y
    def toDouble(x: NoteValue): Double = x.toDouble
    def toFloat(x: NoteValue): Float = x.toFloat
    def toInt(x: NoteValue): Int = x.toInt
    def toLong(x: NoteValue): Long = x.toLong
  }  
}


class Metrum(val base: NoteValue, val count: Int, val name: String = "") {
  val onebar = base * count
  
  override def toString(): String = (if (name != "") name+": " else "") +count + "x"+base
  
  def getBarValue(bars: Int, beats: Int=0): NoteValue = base * (bars * count + beats) 
  def getBarValue(bars: Int, beats: Rational): NoteValue = new NoteValue(onebar * bars + base * beats)
  
  def getNumBars(value: NoteValue): (Int, Rational) = {
    val bars =  (value / onebar).toInt
    val beats = (value - (onebar * bars)) * Rational(count,1)
    ( bars, beats)
  }
  
}

object Metrum {
  def apply(n: Int, m: Int) =  
    new Metrum( new NoteValue(1,m), n, ""+ n + "/" + m)
}

trait Event {
   val value: NoteValue
}

class Rest(val value: NoteValue) extends Event

class NoteEvent[T <: SymbolicNoteBase](val note: T, val value: NoteValue) extends Event

class EventList(val events: List[Event]) extends Event {
  
  val incvalue = events.scanLeft(NoteValue(0,1)){(a,e) => (a + e.value)}
  val value = incvalue(incvalue.size-1) 
  
  
}

