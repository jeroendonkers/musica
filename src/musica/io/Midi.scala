package musica.io
import javax.sound.midi._
import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import scala.collection.JavaConversions._
import musica.symbol._
import musica.math._

trait HasMidiCode {
  val midicode: Int
 }

trait MidiTunedNote extends HasMidiCode {
  val midiFrequency: Double
}

class MidiNote(val midicode: Int) extends HasMidiCode {
   override def toString ="m" + midicode.toString 
}

trait HasMidiInstrument {
  val instcode: Int
}

class MidiInstrument(val instcode: Int) extends HasMidiInstrument {
  override def toString ="instr" + instcode.toString
}

// midi related events

class MidiNoteEvent(i: Int, val value: SymbolicTime) extends Event {
   val event = new MidiNote(i)
   type EventType = MidiNote
   def changeValue(t: SymbolicTime) = {
     new MidiNoteEvent(event.midicode ,t)
  }
}


class InstrumentEvent(i: Int) extends NoTimeEvent {
  val event = new MidiInstrument(i)
  type EventType = MidiInstrument
  val value: SymbolicTime = 0
}

object Midi {
  
  def listDevices(forInput: Boolean): List[String] = {
    val aInfos = MidiSystem.getMidiDeviceInfo();
    aInfos.toList.map(d => {
      try {
        val device = MidiSystem.getMidiDevice(d)
        val allowsInput = (device.getMaxTransmitters() != 0)
        val allowsOutput = (device.getMaxReceivers() != 0)
        if ((allowsInput && forInput) || (allowsOutput && (!forInput)))
				{ d.getName() } else { ""  }       
      } catch {
        case e: Exception => ""
      }
    }).filter(_ != "")

  }
  
  def listInputDevices = listDevices(true)
  def listOutputDevices = listDevices(false)
 
  private def getDevice(s: String): Either[MidiDevice, String] = {
	 try {
			  val aInfos = MidiSystem.getMidiDeviceInfo();
			  val found = aInfos.filter(_.getName == s)
			  if (found.size==0) Right("Device not found") else
			    Left(MidiSystem.getMidiDevice(found.head))
	} catch {  case e: Exception => Right(e.toString) }		
 }
  
  
 private var midiError: String = "" 
 def getMidiError = midiError;
 
 private var midiOutput: Option[MidiDevice] = None
 private var synth: Option[Synthesizer] = None
 private var receiver:  Option[Receiver] = None
 private var sequencer: Option[Sequencer] = None
 private var soundbank: Option[Soundbank] =  None
 private var instrumentnames: List[String] =  List()
 private var instruments: List[Instrument] =  List()
 
  
 def midiOutIsOpen =  receiver.isDefined 
 
 
 def closeMidiOut() = {
   stopPlaying()
   if (receiver.isDefined) { receiver = None }
   if (midiOutput.isDefined) {
        midiOutput.get.close()
        midiOutput = None  
        synth = None
   }
 }
 
 
 // in case of error message,
 //java.util.prefs.WindowsPreferences <init>
 //WARNING: Could not open/create prefs root node Software\JavaSoft\Prefs 
 // at root 0x80000002. Windows RegCreateKeyEx(...) returned error code 5.
 //
 // manually create the following key:
 // HKEY_LOCAL_MACHINE\Software\JavaSoft\Prefs
 
 def openMidiOut(s: String = ""): Boolean = {
   closeMidiOut
   
   midiError = ""
   if (s =="" ) {
     synth = Some(MidiSystem.getSynthesizer())
     midiOutput = Some(synth.get)
     soundbank = Some(synth.get.getDefaultSoundbank())
     instruments = synth.get.getAvailableInstruments().toList
     instrumentnames = instruments.map{ i => i.getName}
   } else { 
     
     getDevice(s) match {
       case Left(d) => midiOutput = Some(d)
       case Right(err) => { midiError= err; midiOutput == None }
   }}  
   
  
   if (midiOutput.isDefined) {
     try {
	   midiOutput.get.open()
	} catch {  case e: Exception => {
	  midiError = e.toString 
	  midiOutput = None
   }}}		
     
   if (midiOutput.isDefined) {    
     receiver = None
     val v = midiOutput.get.getReceiver()
     if (v!=null) { receiver = Some(v) }
   } 
   
   for (i <- 0 to 15) { sendTuningChange(i)} // set all channels to same tuning preset
   receiver.isDefined  
  }
 
 
  def send(m: MidiMessage, t: Long) = {
    if (receiver.isDefined){
      receiver.get.send(m,t)
    }
  }
  
  
  def playSequence(seq: Sequence, bpm: Float = 120) {
    
        val recseq = MidiSystem.getSequencer();
  		recseq.addMetaEventListener(new MetaEventListener() {
				def meta(event: MetaMessage) =
				{
					if (event.getType() == 47) { recseq.close(); sequencer = None}
				}
			});		
  		recseq.open();
  		sequencer = Some(recseq)
  		
  		val lt = recseq.getTransmitters() // close open ends
  		lt.toList.foreach(_.close())
  		val seqTransmitter = recseq.getTransmitter()
  		val r = seqTransmitter.getReceiver()
   		if (r!=null) r.close();  // close open ends
   		
  	    seqTransmitter.setReceiver(receiver.get);
  	    recseq.setSequence(seq);
  	    if (bpm>0) { recseq.setTempoInBPM(bpm) }
  	    recseq.start();
  }
  
  def changeSpeed(bpm: Float) {
     if (sequencer.isDefined) {
       sequencer.get.setTempoInBPM(bpm)
     }
  }
  
  def stopPlaying() {
     if (sequencer.isDefined) {
       sequencer.get.close()
       sequencer = None
     }
    
  }
  
  def changeInstrument(program: Int, channel: Int = 0) {
         if (instruments.size==0) {              
               send(new ShortMessage(ShortMessage.PROGRAM_CHANGE,  channel, program,0),-1)
          } else {
              val myinst = instruments(program)
              val bank = myinst.getPatch().getBank()
              send(new ShortMessage(ShortMessage.CONTROL_CHANGE,  channel, 0, bank / 128),-1)
              send(new ShortMessage(ShortMessage.CONTROL_CHANGE,  channel, 32, bank % 128),-1)
              send(new ShortMessage(ShortMessage.PROGRAM_CHANGE,  channel, myinst.getPatch().getProgram(),0),-1)
          }
  }
  
  
  def play(el: List[TimedEvent], bpm: Float =120, beat: Double = 0.25) = {
    
     val ppq = 10000;
     if (receiver.isDefined) try {
        val sequence = new Sequence(Sequence.PPQ,ppq);
  		val track = sequence.createTrack();
  		el.foreach(e => {
         e.event.event match {
            case  c:  MidiTunedNote => {
           //   println(c.midicode+" "+c.midiFrequency)
               val tun = tuneNote(c.midicode, c.midiFrequency)             
                track.add(new MidiEvent(tun,(e.start.value * (ppq / beat)).toLong))
                val st = new ShortMessage(ShortMessage.NOTE_ON,  0, c.midicode, 100)
                val nd = new ShortMessage(ShortMessage.NOTE_OFF,  0, c.midicode, 0)
                track.add(new MidiEvent(st,(e.start.value * (ppq / beat)).toLong))
                track.add(new MidiEvent(nd,((e.start+e.event.value).value * (ppq / beat)).toLong))   
            
            }
            case  c:  HasMidiCode => {
                val st = new ShortMessage(ShortMessage.NOTE_ON,  0, c.midicode, 100)
                val nd = new ShortMessage(ShortMessage.NOTE_OFF,  0, c.midicode, 0)
                track.add(new MidiEvent(st,(e.start.value * (ppq / beat)).toLong))
                track.add(new MidiEvent(nd,((e.start+e.event.value).value * (ppq / beat)).toLong))   
              
            }
            case  m:  HasMidiInstrument => {
              //  println(m.instcode)
              
                if (instruments.size==0) {              
                  val ins = new ShortMessage(ShortMessage.PROGRAM_CHANGE,  0, m.instcode,0)
                  track.add(new MidiEvent(ins,(e.start.value * (ppq / beat)).toLong))
                } else {
                  val myinst = instruments(m.instcode)
                  val bank = myinst.getPatch().getBank()
                  val bank1 = new ShortMessage(ShortMessage.CONTROL_CHANGE,  0,  0, bank / 128)
                  track.add(new MidiEvent(bank1,(e.start.value * (ppq / beat)).toLong))
                  val bank2 = new ShortMessage(ShortMessage.CONTROL_CHANGE,  0,  0, bank % 128)
                  track.add(new MidiEvent(bank2,(e.start.value * (ppq / beat)).toLong))
                        
                  
                  val ins = new ShortMessage(176,  0, myinst.getPatch().getProgram(),0)
                  track.add(new MidiEvent(ins,(e.start.value * (ppq / beat)).toLong))
                }   
            }
            case _ => {}
          };  
         }) 
  		playSequence(sequence, bpm)
 
     } catch {
       case e: Exception => midiError = e.toString 
     }
    
  }
 
  
def playMidiFile(s: String, bpm: Float = -1) = {
   val file = new FileInputStream(s)
   val is = new BufferedInputStream(file); 
   val sequence = MidiSystem.getSequence(is);
   is.close()
   playSequence(sequence, bpm)
   
}  
  
val tuningpreset = 0  
  
def sendTuningChange(channel: Int) = {
  // Data Entry
  val sm1 = new ShortMessage(); 
  sm1.setMessage(ShortMessage.CONTROL_CHANGE, channel, 0x64, 0x03);
  val sm2 = new ShortMessage();
  sm2.setMessage(ShortMessage.CONTROL_CHANGE, channel, 0x65, 0x00);
  // Tuning program
  val sm3 = new ShortMessage();
  sm3.setMessage(ShortMessage.CONTROL_CHANGE, channel, 0x06, tuningpreset);
  // Data Increment
  val sm4 = new ShortMessage();
  sm4.setMessage(ShortMessage.CONTROL_CHANGE, channel, 0x60, 0x7F);
  // Data Decrement
  val sm5 = new ShortMessage();
  sm5.setMessage(ShortMessage.CONTROL_CHANGE, channel, 0x61, 0x7F);

  send(sm1, -1);
  send(sm2, -1);
  send(sm3, -1);
  send(sm4, -1);
  send(sm5, -1);
 }
  
  // tune (a list) of single notes
  def singleKeyTuning(data: List[(Int,Int,Double)]): MidiMessage = {
    val numdata = data.size
    val sysex: List[Int] = List[Int](0X7F, 0x7F, 0x08, 0x02, tuningpreset, numdata) ++
        data.flatMap({ case (k,l,cents) => {List(k,l, (cents*163.84 / 128).toInt, (cents*163.84 % 128).toInt)  } }) ++
        List(0xF7)
    new SysexMessage(0xF0, sysex.map(_.toByte).toArray, sysex.size)
  }
  
  
  // tune a single note to a given frequency
  def tuneNote(i: Int, freq: Double): MidiMessage = {
    // determine distance from A = 440
       val shift = 1200 * Math.log( freq / 440.0 ) / Math.log(2)
       val numsteps = (shift / 100).floor.toInt
       val notenum = 69 + numsteps
       val centshift = shift - 100*numsteps
       singleKeyTuning(List((i,notenum,centshift)))
  }
  
  
  // default tuning using 12-tone scale tuning message
  def scale12Tuning(tuning: Tuning) = {
    if (tuning.size==12) {
      // use scale tuning message
      val cents = (tuning-Tuning.ET12).centlist 
      val sysex: List[Int] = List[Int](0X7F, 0x7F, 0x08, 0x09, 0x03, 0x7F, 0x7F) ++
      cents.map(c => (c+100)/2).flatMap(c => List((c*163.84 / 128).toInt, (c*163.84 % 128).toInt)) ++
      List(0xF7)
      new SysexMessage(0xF0, sysex.map(_.toByte).toArray, sysex.size)
      
    } else new SysexMessage()
  }
  
    // compute default frequency of midi note i
  def baseFrequency(i: Int) = {
    440.0 * Math.pow(2,(i - 69)/12.0) 
  }
  
  // More complex tuning, allowing more or less than 12 notes and a base frequency.
  // use tuning bulk message
  def scaleTuning(tuning: Tuning, reffreq: Double = 440, refnote: Int = 69) = {
      // get base shift in cents
      val shift = 1200 * Math.log( reffreq / baseFrequency(refnote) ) / Math.log(2)
      // get and shift cents of tuning
      val cents = tuning.centlist.map(_+shift)   
      val n = tuning.size
      val d = n - 60 % n // shift so that key 60 is always start point
       
      val data = List.range(0, 128).map(i => cents((i + d) % n) + 1200 * ((i+d)/n - 60/n  - 1)).
      flatMap(c => {
        val note = (c / 100).floor.toInt 
        if (note+60 <0  || note+60 >127) {
          List(0,0,0)
        } else {
          val cents = c - note*100
          List(note + 60, (cents*163.84 / 128).toInt, (cents*163.84 % 128).toInt)}
        }
      )
      val sysexdata: List[Int] = List[Int](0X7E, 0x7F, 0x08, 0x01, tuningpreset) ++ 
      "0123456789ABCDEF".toArray.map(_.toInt).toList ++ data // add nonsense tuning name
      
      val checksum = (sysexdata.tail.foldLeft(0x7E)((a,b) => a^b)) & 0x7F  // compute checksum by XOR
      
      val sysex = sysexdata ++ List(checksum, 0x7F)
      
      new SysexMessage(0xF0, sysex.map(_.toByte).toArray, sysex.size)
  }
 
  
  def loadSoundBank(path: String) {
    
    if (synth.isDefined) {
    
    if (soundbank.isDefined) {
        synth.get.unloadAllInstruments(soundbank.get)
    }    
    soundbank = Some(MidiSystem.getSoundbank(new File(path)))
    synth.get.loadAllInstruments(soundbank.get)
    instruments = synth.get.getLoadedInstruments().toList
    instrumentnames = instruments.map{ i => i.getName}
    }
  }

  def getInstruments = { instruments }
  
  def getInstrumentnames = { instrumentnames }
}


object GeneralMidi {
 val Instrument = Map( 
	 "Piano 1" -> 0,
	 "Piano 2" -> 1,
	 "Piano 3" -> 2,
	 "Honky Tonk" -> 3,
	 "E.Piano 1" -> 4,
	 "E.Piano 2" -> 5,
	 "Harpsichord" -> 6,
	 "Clavinet" -> 7,
	 "Celesta" -> 8,
	 "Glockenspiel" -> 9,
	 "Music Box" -> 10,
	 "Vibraphone" -> 11,
	 "Marimba" -> 12,
	 "Xylophone" -> 13,
	 "Tubular Bells" -> 14,
	 "Dulcimer" -> 15,
	 "Organ 1" -> 16,
	 "Organ 2" -> 17,
	 "Organ 3" -> 18,
	 "Church Organ" -> 19,
	 "Reed Organ" -> 20,
	 "Accordion" -> 21,
	 "Harmonica" -> 22,
	 "Bandoneon" -> 23,
	 "Nylon Guitar" -> 24,
	 "Steel Guitar" -> 25,
	 "Jazz Guitar" -> 26,
	 "Clean Guitar" -> 27,
	 "Guitar Mutes" -> 28,
	 "Overdrive Guitar" -> 29,
	 "DistortionGuitar" -> 30,
	 "Guitar Harmonics" -> 31,
	 "Acoustic Bass" -> 32,
	 "Fingered Bass" -> 33,
	 "Picked Bass" -> 34,
	 "Fretless Bass" -> 35,
	 "Slap Bass 1" -> 36,
	 "Slap Bass 2" -> 37,
	 "Synth Bass 1" -> 38,
	 "Synth Bass 2" -> 39,
	 "Violin" -> 40,
	 "Viola" -> 41,
	 "Cello" -> 42,
	 "Contrabass" -> 43,
	 "Tremolo Strings" -> 44,
	 "Pizzicato" -> 45,
	 "Harp" -> 46,
	 "Timpani" -> 47,
	 "Strings" -> 48,
	 "Slow Strings" -> 49,
	 "Synth Strings 1" -> 50,
	 "Synth Strings 2" -> 51,
	 "Choir Aahs" -> 52,
	 "Voice Oohs" -> 53,
	 "Synth Vox" -> 54,
	 "Orchestra Hit" -> 55,
	 "Trumpet" -> 56,
	 "Trombone" -> 57,
	 "Tuba" -> 58,
	 "Mute Trumpet" -> 59,
	 "French Horns" -> 60,
	 "Brass" -> 61,
	 "Synth Brass 1" -> 62,
	 "Synth Brass 2" -> 63,
	 "Soprano Sax" -> 64,
	 "Alto Sax" -> 65,
	 "Tenor Sax" -> 66,
	 "Baritone Sax" -> 67,
	 "Oboe" -> 68,
	 "English Horn" -> 69,
	 "Bassoon" -> 70,
	 "Clarinet" -> 71,
	 "Piccolo" -> 72,
	 "Flute" -> 73,
	 "Recorder" -> 74,
	 "Pan Flute" -> 75,
	 "Bottle Chiff" -> 76,
	 "Shakuhachi" -> 77,
	 "Whistle" -> 78,
	 "Ocarina" -> 79,
	 "Square Wave" -> 80,
	 "Saw Wave" -> 81,
	 "Synth Calliope" -> 82,
	 "Chiffer Lead" -> 83,
	 "Charang" -> 84,
	 "Solo Vox" -> 85,
	 "5th Saw Wave" -> 86,
	 "Bass & Lead" -> 87,
	 "Fantasia" -> 88,
	 "Warm Pad" -> 89,
	 "Poly Synth" -> 90,
	 "Space Voice" -> 91,
	 "Bowed Glass" -> 92,
	 "Metal Pad" -> 93,
	 "Halo Pad" -> 94,
	 "Sweep Pad" -> 95,
	 "Ice Rain" -> 96,
	 "Soundtrack" -> 97,
	 "Crystal" -> 98,
	 "Atmosphere" -> 99,
	 "Brightness" -> 100,
	 "Goblin" -> 101,
	 "Echo Drops" -> 102,
	 "Star Theme" -> 103,
	 "Sitar" -> 104,
	 "Banjo" -> 105,
	 "Shamisen" -> 106,
	 "Koto" -> 107,
	 "Kalimba" -> 108,
	 "Bagpipe" -> 109,
	 "Fiddle" -> 110,
	 "Shenai" -> 111,
	 "Tinker Bell" -> 112,
	 "Agogo" -> 113,
	 "Steel Drum" -> 114,
	 "Wood Block" -> 115,
	 "Taiko Drum" -> 116,
	 "Melodic Tom" -> 117,
	 "Synth Drum" -> 118,
	 "Reverse Cymbal" -> 119,
	 "Fret Noise" -> 120,
	 "Breath Noise" -> 121,
	 "Seashore" -> 122,
	 "Bird" -> 123,
	 "Telephone" -> 124,
	 "Helicopter" -> 125,
	 "Applause" -> 126,
	 "Gun Shot" -> 127);
}

  