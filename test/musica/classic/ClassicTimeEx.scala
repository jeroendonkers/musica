package musica.classic

import musica.symbol._
import musica.math._

object ClassicTimeEx {
 
  def main(args: Array[String]): Unit = {
   
    println(Whole)
    
    println(ClassicMetrum(6,8))
    println(ClassicMetrum(2,2))
    println(MetrumC)
    
    
    val l = List( WholeRest, QuarterRest, new ClassicNoteEvent("C",Quarter))
    val e = new EventList(l)
    println(e.value)
    
    
    println( MetrumC.getNumBars(e.value))
    
    val n = MetrumC.getBarValue(5,Rational(7,2))
    println( n)
    println( MetrumC.getNumBars(n))
    
    println(MetrumC.getBarValue(5))
    println(MetrumC.getBarValue(5,3))
    
    
    println(e.incvalue)
    
  }
}