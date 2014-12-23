package musica.symbol
import musica.classic._
import musica.math._
import musica.io._

object SymbolicTimeEx {
    def main(args: Array[String]): Unit = {
     /* 
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
      */
      
      
       Midi.openMidiOut() 
  
      val result = EventListParser("(C:8..- | E^-1- | G-),"+  
                              "(C:2- | E^-1- | G-),"+
                              "(C:Q- | F- | A^-1-),"+
                              "(D^-1:Q' | F' | A^-1'),"+
                              "(D:Q* | F* | G* | B^-1*),"+
                              "(C:H_ | E^-1d[1/1]v[1/2] | G_ | C+1_)",
                              "eitz"
      )
      
      
      result match {
         case Left(e) => { 
            val f = new InstrumentEvent(GeneralMidi.Instrument("Viola")) ++ new Rest(1\8) ++ e
                Midi.play(f.fixAt(0),60)
            println(f.fixAt(0))
         }
         case Right(m) => println(m)
       }  
    }
}