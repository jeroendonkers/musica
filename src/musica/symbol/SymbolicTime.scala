package musica.symbol
import musica.math.Rational
import scala.collection.immutable.ListMap

class SymbolicTime(n: Long, m: Long) extends Rational(n,m) {
  
  def this(r: Rational) = this(r.numer, r.denom)
   
   def +(that: SymbolicTime): SymbolicTime = new SymbolicTime(super.+(that))
   def -(that: SymbolicTime): SymbolicTime = new SymbolicTime(super.-(that))
   def *(that: SymbolicTime): SymbolicTime = new SymbolicTime(super.*(that))
   def /(that: SymbolicTime): SymbolicTime = new SymbolicTime(super./(that))
  
   def >(that: SymbolicTime): Boolean = (that - this).negative
   def <(that: SymbolicTime): Boolean = (this - that).negative 
   def >=(that: SymbolicTime): Boolean = (this == that || this>that)
   def <=(that: SymbolicTime): Boolean = (this == that || this<that)
   
   override def unary_- :SymbolicTime = new SymbolicTime(super.unary_-)
  
      override def equals(that: Any): Boolean = {
     that match {
       case that: SymbolicTime => this.numer == that.numer && this.denom == that.denom
       case _ => false
     }
   }
   
  def *(that: Int): SymbolicTime = { new SymbolicTime(n*that, m) }
  def /(that: Int): SymbolicTime = { new SymbolicTime(n, m*that) }
   
  lazy val dot: SymbolicTime = new SymbolicTime(3*n, 2*m) 
  lazy val dotdot: SymbolicTime =  new SymbolicTime(7*n, 4*m) 
  lazy val triad: SymbolicTime =  new SymbolicTime(n, m*3) 
  
   override def toString = "[" + super.toString + "]"
  
}

object SymbolicTime {
    def apply(n: Long, d: Long = 0) = new SymbolicTime(n, d)  
    def apply(n: Int, d: Int) = new SymbolicTime(n, d) 
    def apply(r: Rational) = new SymbolicTime(r) 
  
  implicit object NoteValueNumeric extends Numeric[SymbolicTime]  {
   // implementing Numeric trait
    def compare(x: SymbolicTime, y: SymbolicTime): Int = {
      if (x.value < y.value) -1 else +1
    }
    def fromInt(x: Int): SymbolicTime = SymbolicTime(x, 1)
    def minus(x: SymbolicTime, y: SymbolicTime): SymbolicTime = x - y
    def negate(x: SymbolicTime): SymbolicTime = -x
    def plus(x: SymbolicTime, y: SymbolicTime): SymbolicTime = x + y
    def times(x: SymbolicTime, y: SymbolicTime): SymbolicTime = x*y
    def toDouble(x: SymbolicTime): Double = x.toDouble
    def toFloat(x: SymbolicTime): Float = x.toFloat
    def toInt(x: SymbolicTime): Int = x.toInt
    def toLong(x: SymbolicTime): Long = x.toLong
  }  
}


class Metrum(val base: SymbolicTime, val count: Int, val name: String = "") {
  val onebar = base * count
  
  override def toString(): String = (if (name != "") name+": " else "") +count + "x"+base
  
  def getBarValue(bars: Int, beats: Int=0): SymbolicTime = base * (bars * count + beats) 
  def getBarValue(bars: Int, beats: Rational): SymbolicTime = new SymbolicTime(onebar * bars + base * beats)
  
  def getNumBars(value: SymbolicTime): (Int, Rational) = {
    val bars =  (value / onebar).toInt
    val beats = (value - (onebar * bars)) * Rational(count,1)
    (bars, beats)
  }
  
}

object Metrum {
  def apply(n: Int, m: Int) =  
    new Metrum( new SymbolicTime(1,m), n, ""+ n + "/" + m)
}

trait Event {
   type EventType
   val event: EventType
   val value: SymbolicTime
   override def toString = event.toString + value
}

class Rest(val value: SymbolicTime) extends Event {
   type EventType = String
   val event: String = "R"
}

class NoteEvent[T <: SymbolicNoteBase](val event: T, val value: SymbolicTime) extends Event {
   type EventType = T
}

class EventList(val event: List[Event]) extends Event {
  
  type EventType = List[Event]
  val incvalue = event.scanLeft(SymbolicTime(0,1)){(a,e) => (a + e.value)}
  val value = incvalue(incvalue.size-1) 
  def eventAtOffset(o: SymbolicTime): Option[Event] = {
    if (o.negative || o >= value) None else
    Some((event.zip(incvalue).filter({case (e,st) => {o >= st && o<st+e.value}}).map({case (e,st)=> e})).head)
  }
  
}

