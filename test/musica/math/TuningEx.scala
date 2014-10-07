package musica.math
import musica.symbol._

object TuningEx {

    def main(args: Array[String]): Unit = {
      
      
      val eq = Tuning("0c,100c,200c,300c,400c,500c,600c,700c,800c,900c,1000c,1100c")
      val agricola = Tuning("135/128,9/8,1215/1024,81/64,4/3,45/32,3/2,405/256,27/16,16/9,243/128");
      val werckmeister3 = Tuning("C0,C#-1,D-1/2,Eb0,E-3/4,F0,G-1/4,G#-1,A-3/4,Bb0,B-3/4") 

      
      println(eq.steplist)
      println(agricola.steplist)
      println(werckmeister3.steplist)
      
 

    }
}