package musica.io
import musica.math._
import musica.symbol._
import musica.classic._


object MidEx extends App {

    Midi.openMidiOut() 
  
     val c1 = new ClassicNoteEvent("C",1\8)
     val c2 = new ClassicNoteEvent("D",1\8)
     val c3 = new ClassicNoteEvent("E",1\8)
     val c4 = new ClassicNoteEvent("F",1\8)
     
     // val event = ((c1 ++ c2) || ( c3 ++ c4)) ** (c1 ++ c2 ++ c3)
    
  
     val d1 = new MidiNoteEvent(58,1\4)
     val d2 = new MidiNoteEvent(63,1\4)
     val d3 = new MidiNoteEvent(66,1\4)
     val d4 = new MidiNoteEvent(69,1\4)  
      
     //val event = (((c1 ++ c2 ++ c3 ++ c4) * 2) || (d1 ++ d2 ++ d3 ++ d4)) * 16
     
     val event = new EventList(List.range(1,12).map(i => new MidiNoteEvent(59+i,1\4)))
     val el = event.fixAt(0)
   //   println(el)
      
   // val m = Midi.singleKeyTuning(List((62,60,20),(64,60,50),(65,60,75))) 
    
    val agricola = Tuning("1/1,135/128,9/8,1215/1024,81/64,4/3,45/32,3/2,405/256,27/16,16/9,243/128");
    val werckmeister3 = Tuning("C^0,C#^-1,D^-1/2,Eb^0,E^-3/4,F^0,F#^-1,G^-1/4,G#^-1,A^-3/4,Bb^0,B^-3/4") 
   
    FifthTuning.loadXML("./data/werckmeister4.FifthTuning_Musica_xml") match {
       case Left(t) =>    Midi.send(Midi.scaleTuning(t),-1) 
        case Right(s) => println(s)
     }


  //  Midi.playMidiFile("data/bwv988.mid",70)
    
   // val test = Tuning.ET(31)
  //  println(test.centlist)
    //Midi.send(Midi.scaleTuning(test),-1)
    Midi.playMidiFile("data/bwv988.mid",70) 
    
}