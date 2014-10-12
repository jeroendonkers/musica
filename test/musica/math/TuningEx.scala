package musica.math
import musica.symbol._

object TuningEx {

    def main(args: Array[String]): Unit = {
      
      
      val eq = Tuning.ET12
      val agricola = Tuning("1/1,135/128,9/8,1215/1024,81/64,4/3,45/32,3/2,405/256,27/16,16/9,243/128");
      val werckmeister3 = Tuning("C^0,C#^-1,D^-1/2,Eb^0,E^-3/4,F^0,F#^-1,G^-1/4,G#^-1,A^-3/4,Bb^0,B^-3/4") 
      val architas = Tuning.fromRatios("28/27,8/7,9/8,28/27,8/7,9/8")
      
      
      
      println(eq.steplist)
      println(agricola.steplist)
      println(werckmeister3.steplist)
      println(architas.steplist) 
 
      println(eq.centlist)
      println(agricola.centlist)
      println(werckmeister3.centlist)
      println(architas.centlist) 
      
      println(eq.steplist)
      
      println ((agricola - eq).centlist)  
      println (agricola.intervals(7).steplist)
     
      println ((werckmeister3 - eq).centlist)
      println ((werckmeister3.intervals(7) - Tuning(PureInterval.Fifth,12)).centlist)
      println ((werckmeister3.intervals(4) - Tuning(PureInterval.MajorThird,12)).centlist)
      
      val qrt = Rational(-1,4)
      val meantone = Tuning.fromFifths("Eb,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4")
      println(meantone.steplist)
      val sauveur = Tuning.fromFifths("Eb,-1/5,-1/5,-1/5,-1/5,-1/5,-1/5,-1/5,-1/5,-1/5,-1/5,-1/5")
      println(sauveur.steplist)
      
      val bachkellner = Tuning.fromFifths("C,-1/5,-1/5,-1/5,-1/5,0,-1/5,0,0,0,0,0", PureInterval.PythagoreanComma)
      println(bachkellner.steplist)
      
    }
}