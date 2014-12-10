package musica.classic

import musica.symbol._
import musica.math._

object ClassicTimeEx {
 
  def main(args: Array[String]): Unit = {
   
    println(Whole)
    
    println(ClassicMetrum(6,8))
    println(ClassicMetrum(2,2))
    println(MetrumC)
    
    
    val l = List( new EitzEvent(EitzInterval.pure("C^+1"), Whole,1 ), QuarterRest, new ClassicNoteEvent("C",Quarter))
    val e = new EventList(l)
   
    
    
    println( MetrumC.getNumBars(e.value))
    
    val n = MetrumC.getBarValue(5,Rational(7,2))
    println( n)
    println( MetrumC.getNumBars(n))
    
    println(MetrumC.getBarValue(5))
    println(MetrumC.getBarValue(5,3))
    
     println(e)
    println(e.incvalue)
    
    println(e.eventsAtOffset(Whole + Sixteenth * 5))
    
    
    val x =  (e ++ new ClassicNoteEvent("C",Quarter)) || (WholeRest ++ e)
    
    println(x)
    println(x.eventsAtOffset(Sixteenth * 3))
    
    println(x.fixAt())
    println(x.count)
    
    
     val r1 = WholeRest
     val r2 = QuarterRest
     val c1 =  new NoteEvent[ClassicNote]("C",Quarter,1)
     val c2 =  new NoteEvent[ClassicNote]("D",Quarter,1)
     val empty = new EventList(List())
     
     val event = empty ++ ((c1 ++ r2) || (r1 ++ c2 ++ empty)) 
     println(event)
     
     val fixed = event.fixAt(SymbolicTime(1,4))
     
     println(fixed)
     println(empty.fixAt())
     
     println( empty * 5)
     
     val rep = x *** (!(r1 ++ r2) ++ c1)
    
     println(rep)
     println(rep.fixAt(Quarter))
     
     
     println( !(r1 ++ r2) ++ !(c1++c2) ++ !(c1++r1) )
     
    println(ClassicEventParser.eitz("C# ^ -1"))
  }
}