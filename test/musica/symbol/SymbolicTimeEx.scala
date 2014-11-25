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
  
      val e = EventListParser("(C[1/4] | E^-1[1/4] | G[1/4]),"+  
                              "(C[1/4] | F[1/4] | A^-1[1/4]),"+
                              "(D^-1[1/4] | F[1/4] | A^-1[1/4]),"+
                              "(D[1/4] | F[1/4] | G[1/4] | B^-1[1/4]),"+
                              "(C[1/2] | E^-1[1/2] | G[1/2] | C+1[1/2])"
      )
      
      
      
      if (e.isDefined) { 
        val f = new InstrumentEvent(GeneralMidi.Instrument("Violin")) ++ e.get
        //val f = (new EitzEvent(EitzInterval.pure("D"),1\4)) ++ e.get
        Midi.play(f.fixAt(0),60)
       println(f.fixAt(0))
      }
    }
}