package musica.io
import musica.math._
import musica.symbol._
import musica.classic._
import java.io.File;




object MidEx extends App {
  
    Midi.openMidiOut() 
  
     val c1 = EitzEvent("C",1\4)
     val c2 = EitzEvent("E^-1",1\4)
     val c3 = EitzEvent("G^0",1\4)

     
     // val event = ((c1 ++ c2) || ( c3 ++ c4)) ** (c1 ++ c2 ++ c3)
    
  
     val d1 = new MidiNoteEvent(58,1\4)
     val d2 = new MidiNoteEvent(63,1\4)
     val d3 = new MidiNoteEvent(66,1\4)
     val d4 = new MidiNoteEvent(69,1\4)  
      
     //val event = (((c1 ++ c2 ++ c3 ++ c4) * 2) || (d1 ++ d2 ++ d3 ++ d4)) * 16   
  //   val event = new EventList(List.range(1,12).map(i => new MidiNoteEvent(59+i,1\4)))
    
    
     val inst = new InstrumentEvent(GeneralMidi.Instrument("Violin")) 
     val event = inst ++ c1 ++ c2 ++ c3 ++ (( c1 || c2 || c3) * 3)
    // println(event)
     val el = event.fixAt(0)
   //  for( i<- (0 to 13)) {Midi.send(Midi.tuneNote(59+i,440+i*5),-1)}
     
     Midi.play(el)
     
     
     
 //   val info  = AudioSystem.getMixerInfo()
   // info.foreach(i => println(i.getDescription().toString))
     

 
     
 /*    
   //   println(el)

   
     val agricola = Tuning("1/1,135/128,9/8,1215/1024,81/64,4/3,45/32,3/2,405/256,27/16,16/9,243/128");
    val werckmeister3 = Tuning("C^0,C#^-1,D^-1/2,Eb^0,E^-3/4,F^0,F#^-1,G^-1/4,G#^-1,A^-3/4,Bb^0,B^-3/4") 
   
    FifthTuning.loadXML("./data/werckmeister4.FifthTuning_Musica_xml") match {
       case Left(t) =>    Midi.send(Midi.scaleTuning(t, 440,69),-1) 
        case Right(s) => println(s)
     }


    
   // val test = Tuning.ET(31)
  //  println(test.centlist)
   // Midi.send(Midi.scaleTuning(agricola,415),-1)
    
 //  Midi.playMidiFile("data/bwv988.mid",70) 
*/
    
}