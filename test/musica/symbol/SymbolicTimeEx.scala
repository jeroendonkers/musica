package musica.symbol
import musica.classic._
import musica.math._
import scala.util.parsing.combinator._

object SymbolicTimeEx {
    def main(args: Array[String]): Unit = {
      
     val r1 = WholeRest
     val r2 = QuarterRest
     val c1 =  new NoteEvent[ClassicNote]("C",Quarter)
     val c2 =  new NoteEvent[ClassicNote]("D",1\4)
     
      val event = ((c1 ++ r2) || (r1 ++ c2))  *** (r1 ++ r2)
     //val event = (c1 || c2)  *** (r1 ++ r2)
      val x = new EventMap(event)
      
     
      println(event.asEventList)
      println(event.fixAt(0))
      println (x.startmap)
      println (x.endmap)
      
      
      println(EventParser("(C,D[1/4],C^+1)"))
      
    }
}