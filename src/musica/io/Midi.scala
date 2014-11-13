package musica.io
import javax.sound.midi._
import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import scala.collection.JavaConversions._
import musica.symbol._
import musica.math._


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
 private var receiver:  Option[Receiver] = None
 private var sequencer: Option[Sequencer] = None
  
 def midiOutIsOpen =  receiver.isDefined 
 
 
 def closeMidiOut() = {
   if (receiver.isDefined) { receiver = None }
   if (midiOutput.isDefined) {
        midiOutput.get.close()
        midiOutput = None     
   }
 }
 
 
 // in case of error message,
 //java.util.prefs.WindowsPreferences <init>
 //WARNING: Could not open/create prefs root node Software\JavaSoft\Prefs 
 // at root 0x80000002. Windows RegCreateKeyEx(...) returned error code 5.
 //
 // manually create the following key:
 // HKEY_LOCAL_MACHINE\Software\JavaSoft\Prefs
 
 def openMidiOut(s: String = "Gervill"): Boolean = {
   closeMidiOut
   midiError = ""
   getDevice(s) match {
     case Left(d) => midiOutput = Some(d)
     case Right(err) => { midiError= err; midiOutput == None }
   }  
   
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
  	    recseq.setTempoInBPM(bpm)
  	    recseq.start();
  }
  
  def stopPlaying() {
     if (sequencer.isDefined) {
       sequencer.get.close()
       sequencer = None
     }
    
  }
  
  def play(el: List[TimedEvent], bpm: Float =120, beat: Double = 0.25) = {
    
     val ppq = 10000;
     if (receiver.isDefined) try {
        val sequence = new Sequence(Sequence.PPQ,ppq);
  		val track = sequence.createTrack();
  		el.foreach(e => {
          e.event.event match {
            case  c:  HasMidiCode => {
                val st = new ShortMessage(ShortMessage.NOTE_ON,  0, c.midicode, 127)
                val nd = new ShortMessage(ShortMessage.NOTE_OFF,  0, c.midicode, 0)
                track.add(new MidiEvent(st,(e.start.value * (ppq / beat)).toLong))
                track.add(new MidiEvent(nd,((e.start+e.event.value).value * (ppq / beat)).toLong))   
            }		
          }}) 
  		playSequence(sequence, bpm)
 
     } catch {
       case e: Exception => midiError = e.toString 
     }
    
  }
 
  
def playMidiFile(s: String, bpm: Float = 120) = {
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
  
  
  def singleKeyTuning(data: List[(Int,Int,Double)]): MidiMessage = {
    val numdata = data.size
    val sysex: List[Int] = List[Int](0X7F, 0x7F, 0x08, 0x02, tuningpreset, numdata) ++
        data.flatMap({ case (k,l,cents) => {List(k,l, (cents*163.84 / 128).toInt, (cents*163.84 % 128).toInt)  } }) ++
        List(0xF7)
    new SysexMessage(0xF0, sysex.map(_.toByte).toArray, sysex.size)
  }
  
  def scaleTuning(tuning: Tuning) = {
    val cents = (tuning-Tuning.ET12).centlist 
    val sysex: List[Int] = List[Int](0X7F, 0x7F, 0x08, 0x09, 0x03, 0x7F, 0x7F) ++
    cents.map(c => (c+100)/2).flatMap(c => List((c*163.84 / 128).toInt, (c*163.84 % 128).toInt)) ++
    List(0xF7)
    new SysexMessage(0xF0, sysex.map(_.toByte).toArray, sysex.size)
  }
  
  
}