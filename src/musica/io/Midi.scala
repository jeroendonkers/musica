package musica.io
import javax.sound.midi._
import scala.collection.JavaConversions._
import musica.symbol._


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
 
 def openMidiOut(s: String): Boolean = {
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
   receiver.isDefined  
  }
 
 
  def send(m: MidiMessage, t: Long) = {
    if (receiver.isDefined){
      receiver.get.send(m,t)
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
  		
  		val recseq = MidiSystem.getSequencer();
  		recseq.addMetaEventListener(new MetaEventListener() {
				def meta(event: MetaMessage) =
				{
					if (event.getType() == 47) { recseq.close(); }
				}
			});		
  		recseq.open();
  		
  		val lt = recseq.getTransmitters() // close open ends
  		lt.toList.foreach(_.close())
  		val seqTransmitter = recseq.getTransmitter()
  		val r = seqTransmitter.getReceiver()
   		if (r!=null) r.close();  // close open ends
   		
  	    seqTransmitter.setReceiver(receiver.get);
  	    recseq.setSequence(sequence);
  	    recseq.setTempoInBPM(bpm)
  	    recseq.start();
     } catch {
       case e: Exception => midiError = e.toString 
     }
    
  }
  
}