package musica.symbol
import musica.math._
import musica.io.MidiNote
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
   
   def eventsStartAtOffset(o: SymbolicTime): List[TimedEvent] = {
      if (o != 0) List() else List(TimedEvent(this,0\1))
   }
   
   def eventsEndAtOffset(o: SymbolicTime): List[TimedEvent] = {
      if (o != value) List() else List(TimedEvent(this,0\1))
   }
   
   
   def fixAt(o: SymbolicTime = SymbolicTime.zero): List[TimedEvent] = {
      List(TimedEvent(this,o))
   }
   
   def getEventList: List[Event] = {
      this match {
         case e: EventList => e.event
         case e: Event => List(e)
      }   
   }
   
   def getEventBlock: List[Event] = {
      this match {
         case e: EventBlock => e.event
         case e: Event => List(e)
      }   
   }
  
     // operator ++ concatenate events: create or merge EventLists
     def ++(that: Event): EventList = new EventList(this.getEventList ++ that.getEventList)
     
     
     // keep as list : use this to prevent merging into a list when using ++ operator
     def unary_! : EventList = new EventList(List(this))
     
     // operator || put events into a block
     def ||(that: Event): EventBlock = new EventBlock(this.getEventBlock ++ that.getEventBlock)
  
     // operator *: repeat events (concatenate in case of eventlist)
     def *(that: Int): EventList = {
       if (that <=0) new EventList(List()) else 
       new EventList((List.fill(that)(getEventList)).flatten)         
     }
     
     // generate repeat event, no da capo, use ! operator in right-hand expression 
      def **(that: Event): RepeatEvent = new RepeatEvent(List(this) ++ that.getEventList)
      
     // generate repeat event with da capo, use ! operator in right-hand expression
      def ***(that: Event): RepeatEvent = new RepeatEvent(List(this) ++ that.getEventList, true)
      
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
  
  private [this] def findEvent(o: SymbolicTime) = {
     (event.zip(incvalue).filter({case (e,st) => {o >= st && o<st+e.value}})).head
  }
  
  override def eventsAtOffset(o: SymbolicTime): List[TimedEvent] = {
    if (o.negative || o >= value) List() else {
    val (e,st) = findEvent(o)
    e.eventsAtOffset(o - st)
    }
  }
  
  override def eventsStartAtOffset(o: SymbolicTime): List[TimedEvent] = {
    if (o.negative || o >= value) List() else {
    val (e,st) = findEvent(o)
    e.eventsStartAtOffset(o - st)
    }
  }
  
   override def eventsEndAtOffset(o: SymbolicTime): List[TimedEvent] = {
    if (o.negative || o >= value) List() else {
    val (e,st) = findEvent(o)
    e.eventsEndAtOffset(o - st)
    }
  }
  
   override def fixAt(o: SymbolicTime): List[TimedEvent] = {
    event.zip(incvalue).flatMap({case (e,st) => e.fixAt(o+st)})
  }
}

// synchronous events: all events start at the same time, but the duration (value) may differ
class EventBlock(val event: List[Event]) extends CompositeEvent {
  
  type EventType = List[Event]
  val value = event.map(e => e.value).max 
  override val count = event.map(e => e.count).sum
  
  override def eventsAtOffset(o: SymbolicTime): List[TimedEvent] = {
    if (o.negative || o >= value) List() else {
      event.flatMap(e => e.eventsAtOffset(o))
    }
  }
  
  override def eventsStartAtOffset(o: SymbolicTime): List[TimedEvent] = {
    if (o.negative || o >= value) List() else {
      event.flatMap(e => e.eventsStartAtOffset(o))
    }
  }
  
   override def eventsEndAtOffset(o: SymbolicTime): List[TimedEvent] = {
    if (o.negative || o >= value) List() else {
      event.flatMap(e => e.eventsEndAtOffset(o))
    }
  }
   
   override def fixAt(o: SymbolicTime): List[TimedEvent] = {
      event.flatMap(e => e.fixAt(o)).sortWith(TimedEvent.compare)
   }
}

// repeat event. The first element in the list is the (composite) event that needs to repeated
// the other elements in the list are each merged between the repeats and so determine the amount of repeats.
// if daCapo is true, the first element is repeated at the end.
// Useable for repeats, dacapo aria. rounds, rondeau's and so on
class RepeatEvent(val event: List[Event], val daCapo: Boolean = false) extends CompositeEvent {
   type EventType = List[Event]
   
   lazy val asEventList: EventList = {
     if (event.size==0) new EventList(List()) else {
       val head = event.head.getEventList
       val v = event.tail.map(e => head ++ e.getEventList).flatten ++ (if (daCapo) head else List())
       new EventList(v)
     }
   }
   val value = {
       if (event.size==0) SymbolicTime.zero else {
         event.tail.map(e => e.value).sum + event.head.value * (event.size - (if (daCapo) 0 else 1))
       }
   }
    override val count: Int = {
       if (event.size==0) 0 else {
         event.tail.map(e => e.count).sum + event.head.count * (event.size - (if (daCapo) 0 else 1))
       }
   }
     
    override def eventsAtOffset(o: SymbolicTime): List[TimedEvent] = asEventList.eventsAtOffset(o)
    override def eventsStartAtOffset(o: SymbolicTime): List[TimedEvent] = asEventList.eventsStartAtOffset(o) 
    override def eventsEndAtOffset(o: SymbolicTime): List[TimedEvent] = asEventList.eventsEndAtOffset(o) 
    
    override def fixAt(o: SymbolicTime): List[TimedEvent] = asEventList.fixAt(o)

}


class TimedEvent(val event: Event, val start: SymbolicTime = SymbolicTime.zero) {
   def +(that: SymbolicTime): TimedEvent = new TimedEvent(event, start+that)
   val end = start + event.value
   override def toString = ""+start+":"+event.toString    
}

object TimedEvent {
  def apply(event: Event, start: SymbolicTime = SymbolicTime.zero) = new TimedEvent(event,start)
  def compare(a: TimedEvent, b:  TimedEvent): Boolean = a.start < b.start
}


class EventMap(val event: Event)  {
     private val fixmap = event.fixAt(0)
     private def condense(l: List[TimedEvent], n: List[(SymbolicTime,List[Event])],
       f: TimedEvent => SymbolicTime): List[(SymbolicTime,List[Event])] = {
     l match {
       case List() => n
       case h :: t => {
         val target = f(h) 
         n match {
         case List() => condense(t, List((target, List(h.event))),f) 
         case x :: y => if (x._1 == target) condense(t, List((x._1, x._2 ++ List(h.event))) ::: y,f )
                        else condense(t, List((target, List(h.event))) ::: n,f )
       }}       
     }    
   }
   private def createListMap(a: List[(SymbolicTime,List[Event])]) = 
        a.foldLeft(ListMap[SymbolicTime,List[Event]]()){  (m,s) => m + s } 
   
   val startmap = createListMap(condense(fixmap, List(), _.start).reverse)
   val endmap = createListMap(condense(fixmap, List(), _.end).reverse)
}