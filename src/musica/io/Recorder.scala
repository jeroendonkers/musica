package musica.io
import javax.sound.sampled._;
import java.io.File

class Recorder(strFilename: String) extends Thread {

     private var dataTargetLine: TargetDataLine = null
     private var targetType: AudioFileFormat.Type = null
     private var audioInputStream: AudioInputStream = null
    private var outputFile: File = null
    private var mixer: Mixer = null 

   
          outputFile = new File(strFilename);

          val audioFormat = new AudioFormat(AudioFormat.Encoding.PCM_SIGNED, 44100.0F, 16, 2, 4, 44100.0F, false);

          val info = new DataLine.Info(classOf[TargetDataLine], audioFormat);
          
          try{
             //  val mixerinfo = AudioSystem.getMixerInfo()(5)
           //    mixer = AudioSystem.getMixer(mixerinfo)
              // val port = mixer.getLine(info).asInstanceOf[Port];
               dataTargetLine =  AudioSystem.getLine(info).asInstanceOf[TargetDataLine];
               dataTargetLine.open(audioFormat);
          }
          catch {
            case e: LineUnavailableException => {
               e.printStackTrace();
               System.exit(1);
            }
          }

          targetType = AudioFileFormat.Type.WAVE;     
     
    
     
     def stopRecording(){
          dataTargetLine.stop();
          dataTargetLine.close();
     }
     
     
     
     def  startRecording(){
          start(); // Starts a new Thread to record the audio data     
     }
     
     
     
     override def run  {
          
          try {
               audioInputStream = new AudioInputStream(dataTargetLine);
               dataTargetLine.start();
               AudioSystem.write(audioInputStream,targetType,outputFile);

          } catch {
            case e: Exception => {
               e.printStackTrace();
            }   
          }
     }
}         
     
     object Recorder {
              
         var recorder: Option[Recorder] = None
         
         def startRecording(strFilename: String) {
           if (recorder.isDefined) { recorder.get.stopRecording }
           recorder = Some(new Recorder(strFilename))
           recorder.get.startRecording
         }
         
         def stopRecording() {
           if (recorder.isDefined) { recorder.get.stopRecording }
           recorder = None
         }
       
         def showMixers() {
  
    	  val sourceDLInfo = new Line.Info(classOf[SourceDataLine]);
    	  val targetDLInfo = new Line.Info(classOf[TargetDataLine]);
    	  val clipInfo = new Line.Info(classOf[Clip]);
    	  val portInfo = new Line.Info(classOf[Port]);

    	  AudioSystem.getMixerInfo().foreach(mixInfo => {
   		     val mixer = AudioSystem.getMixer(mixInfo);
    		 var support = " supports ";
    		 if (mixer.isLineSupported(sourceDLInfo))
    			support += 	"SourceDataLine ";
    		if (mixer.isLineSupported(clipInfo))
    			support += " Clip ";
    		if (mixer.isLineSupported(targetDLInfo))
    			support += " TargetDataLine ";
    		if (mixer.isLineSupported(portInfo))
    			support += " Port ";
    		  println("Mixer: ["	+ mixInfo.getName() + "]"+
    			support + " = " +
    			mixInfo.getDescription())});
    }  
     
     
    def probePort() {
      val portInfo = new Line.Info(classOf[Port]);
      AudioSystem.getMixerInfo().foreach( mixerInfo => { 
           val mixer = AudioSystem.getMixer(mixerInfo); 
           if (mixer.isLineSupported(portInfo)) {
    	// found a Port Mixer
    	  print("Found mixer: "  + mixerInfo.getName());
    	  println(" = " + mixerInfo.getDescription());
    	  println("=> Source Line Supported:");
    	  mixer.getSourceLineInfo().foreach(srcInfo => {
    		val pi =   srcInfo.asInstanceOf[Port.Info];
    		println (" ==> " + pi.getName() +
    			", " + (if (pi.isSource()) "source" else "target"));
    		showControls(mixer.getLine(srcInfo));
    	   })
    	println("=> Target Line Supported:");
        mixer.getTargetLineInfo().foreach(targetInfo => {
    		val pi = targetInfo.asInstanceOf[Port.Info];
    		println(" ==> " + pi.getName()
    			+ ", " +
    			(if (pi.isSource()) "source" else "taget"));
    		showControls(mixer.getLine(targetInfo));
    		})
           }	
    	}) 
    }   
 
    private def showControls(inLine: Line)  {
      inLine.open();
      println("    ====> Available controls:");
      inLine.getControls().foreach( ctrl => {
    	println( "        " + ctrl.toString());
    	if (ctrl.isInstanceOf[CompoundControl])  {
    	  val cc = ctrl.asInstanceOf[CompoundControl]
    	  cc.getMemberControls().foreach(ictrl => println(" "+ ictrl.toString()))
    	}})
      println()	
      inLine.close();
    }
    
 
 }