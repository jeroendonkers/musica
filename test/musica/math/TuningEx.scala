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
      val meantone = Tuning.fromFifths("Eb,S,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4")
      println(meantone.centmap)
      val sauveur = Tuning.fromFifths("Eb,S,-1/5,-1/5,-1/5,-1/5,-1/5,-1/5,-1/5,-1/5,-1/5,-1/5,-1/5")
      println(sauveur.centmap)
      
      val bachkellner = Tuning.fromFifths("C,P,-1/5,-1/5,-1/5,-1/5,0,-1/5,0,0,0,0,0")
      println(bachkellner.centmap)
      
      println(meantone.compare(ClassicInterval.MajorThird,PureInterval.MajorThird).centmap )
      println(sauveur.compare(ClassicInterval.MajorThird,PureInterval.MajorThird).centmap )
      
      println(meantone.frequency(0, "A",415))
      
      println(meantone.frequencies(List.range(0,12), "A",415))
      
                      
      meantone.exportHauptwerk("./data",HauptwerkSpecs("1/4 syntonic comma meantone","1/4 meantone", "quarter_meantone",
                     "800001","1.0"))  
                                 
      bachkellner.exportHauptwerk("./data",HauptwerkSpecs("Bach Kellner proposed","Kellner", "kellner",
                     "800002","1.0"))                 
    }
}