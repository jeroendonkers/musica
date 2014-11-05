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
    
    val zero = SymbolicTime(0)
  
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

abstract class Event {
   type EventType
   val event: EventType
   val value: SymbolicTime
   val count: Int = 1
   override def toString = event.toString + value
   
   def eventsAtOffset(o: SymbolicTime): List[TimedEvent] = {
      if (o.negative || o >= value) List() else List(TimedEvent(this,-o))
   }
   
   def fixAt(o: SymbolicTime = SymbolicTime.zero): List[TimedEvent] = {
      List(TimedEvent(this,o))
   }
   
     // operator ++ concatenate events: create or merge EventLists
     def ++(that: Event): Event = { 
       this match {
         case e: EventList => that match {
            case f: EventList => new EventList(e.event ++ f.event)
            case f: Event =>  new EventList(e.event ++ List(f))
           } 
         case e: Event => that match {
            case f: EventList => new EventList(List(e) ++ f.event)
            case f: Event =>  new EventList(List(e,f))
           }   
       }
     }
   
     // operator || put events into a block
      def ||(that: Event): Event = { 
       this match {
         case e: EventBlock => that match {
            case f: EventBlock => new EventBlock(e.event ++ f.event)
            case f: Event =>  new EventBlock(e.event ++ List(f))
           } 
         case e: Event => that match {
            case f: EventBlock => new EventBlock(List(e) ++ f.event)
            case f: Event =>  new EventBlock(List(e,f))
           }   
       }
     }
}

class Rest(val value: SymbolicTime) extends Event {
   type EventType = String
   val event: String = "R"
}

class NoteEvent[T <: SymbolicNoteBase](val event: T, val value: SymbolicTime) extends Event {
   type EventType = T
}

abstract class CompositeEvent extends Event

// serial events
class EventList(val event: List[Event]) extends CompositeEvent {
  
  type EventType = List[Event]
  val incvalue = event.scanLeft(SymbolicTime(0,1)){(a,e) => (a + e.value)}
  val value = incvalue(incvalue.size-1)
  override val count = event.map(e => e.count).sum
  
  override def eventsAtOffset(o: SymbolicTime): List[TimedEvent] = {
    if (o.negative || o >= value) List() else {
    val (e,st) = (event.zip(incvalue).filter({case (e,st) => {o >= st && o<st+e.value}})).head
    e.eventsAtOffset(o - st)
    }
  }
  
   override def fixAt(o: SymbolicTime): List[TimedEvent] = {
    event.zip(incvalue).flatMap({case (e,st) => e.fixAt(o+st)})
  }
}

// synchronous events
class EventBlock(val event: List[Event]) extends CompositeEvent {
  
  type EventType = List[Event]
  val value = event.map(e => e.value).max 
  override val count = event.map(e => e.count).sum
  
  override def eventsAtOffset(o: SymbolicTime): List[TimedEvent] = {
    if (o.negative || o >= value) List() else {
      event.flatMap(e => e.eventsAtOffset(o))
    }
  }
  
   override def fixAt(o: SymbolicTime): List[TimedEvent] = {
      event.flatMap(e => e.fixAt(o)).sortWith(TimedEvent.compare)
   }
}


class TimedEvent(val event: Event, val start: SymbolicTime = SymbolicTime.zero) {
   def +(that: SymbolicTime): TimedEvent = new TimedEvent(event, start+that)
   override def toString = ""+start+":"+event.toString    
}

object TimedEvent {
  def apply(event: Event, start: SymbolicTime = SymbolicTime.zero) = new TimedEvent(event,start)
  def compare(a: TimedEvent, b:  TimedEvent): Boolean = a.start < b.start
}
