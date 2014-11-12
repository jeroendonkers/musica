package musica.io
import musica.math._
import musica.symbol._
import musica.classic._


object MidEx extends App {

     val c1 = new ClassicNoteEvent("C",1\4)
     val c2 = new ClassicNoteEvent("D",1\4)
     val c3 = new ClassicNoteEvent("E",1\4)
     val c4 = new ClassicNoteEvent("F",1\4)
     
     // val event = ((c1 ++ c2) || ( c3 ++ c4)) ** (c1 ++ c2 ++ c3)
     val event = c1 ++ c2 ++ c3 ++ c4
     val el = event.fixAt(0)
      println(el)
  
     
    val m = Midi.singleKeyTuning(List((62,60,50),(64,61,0),(65,61,50))) 
   
      
    println(Midi.openMidiOut())  
 //   Midi.sendTuningChange(0)
    Midi.send(m,-1) 
    Midi.play(el, 60)
    println("hallo")
    
}