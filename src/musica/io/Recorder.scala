package musica.io
import javax.sound.sampled._;
import java.io.File

class Recorder(strFilename: String) extends Thread {

     private var dataTargetLine: TargetDataLine = null
     private var targetType: AudioFileFormat.Type = null
     private var audioInputStream: AudioInputStream = null
    private var outputFile: File = null

   
          outputFile = new File(strFilename);

          val audioFormat = new AudioFormat(AudioFormat.Encoding.PCM_SIGNED, 44100.0F, 16, 2, 4, 44100.0F, false);

          val info = new DataLine.Info(classOf[TargetDataLine], audioFormat);
          
          try{
               dataTargetLine = AudioSystem.getLine(info).asInstanceOf[TargetDataLine];
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