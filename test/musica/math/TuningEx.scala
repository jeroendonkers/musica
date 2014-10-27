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
      println(architas.steplist) // List(1/1, 28/27, 32/27, 4/3, 112/81, 128/81, 16/9)
 
      println(eq.centlist)
      println(agricola.centlist)
      println(werckmeister3.centlist)
      println(architas.centlist) // List(0.0, 62.96090387296258, 294.13499740383764, 498.04499913461245, ...
      
      println(eq.steplist)
      
      println ((agricola - eq).centlist)  
      println (agricola.intervals(7).steplist)
     
      println ((werckmeister3 - eq).centlist) //List(0.0, -7.82128353900292, 3.910001730774866, -3.911281808
      println ((werckmeister3.intervals(7) - Tuning(PureInterval.Fifth,12)).centlist) //List(-5.376572399178713, 0.0, -5.3765723991
      println ((werckmeister3.intervals(4) - Tuning(PureInterval.MajorThird,12)).centlist)
      
      
     
      val qrt = Rational(-1,4)
      val meantone = FifthTuning("Eb,S,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4").mappedTuning
      println(meantone.centmap)
      val sauveur = FifthTuning("Eb,S,-1/5,-1/5,-1/5,-1/5,-1/5,-1/5,-1/5,-1/5,-1/5,-1/5,-1/5").mappedTuning
      println(sauveur.centmap)
      
      val bachkellner = FifthTuning("C,P,-1/5,-1/5,-1/5,-1/5,0,-1/5,0,0,0,0,0")
      println(bachkellner.mappedTuning.centmap)
      
      println(meantone.compare(ClassicInterval.MajorThird,PureInterval.MajorThird).centmap )
      println(sauveur.compare(ClassicInterval.MajorThird,PureInterval.MajorThird).centmap )
      
      println(meantone.frequency(0, "A",415))
      
      println(meantone.frequencies(List.range(0,12), "A",415))
      
                      
      meantone.exportHauptwerk("./data","1/4 syntonic comma meantone","1/4 meantone", "quarter_meantone",
                     "800001","1.0")
                                 
      bachkellner.mappedTuning.exportHauptwerk("./data","Bach Kellner proposed","Kellner", "kellner",
                     "800002","1.0")
      bachkellner.save("./data","Bach Kellner proposed","bachkellner")     
         
       meantone.save("./data","1/4 syntonic comma meantone","quarter_meantone")
        agricola.save("./data","Agricola","agricola")
        
       werckmeister3.exportScl("./data","wervkmeisterIII","Werckmeister III") 

       val mp = new ClassicMappedTuning(werckmeister3.steplist, ClassicScale.Chromatic,ClassicNote("C"),"")
       println(mp.centmap) //List((C,0.0), (Db,92.17871646099708), (D,193.15685693241744), (Eb,294.134
       println(mp.intervals(ClassicInterval.Fifth).centmap) // List((C,696.5784284662087), (Db,701.9550008653874), (D,696.578
       println(mp.compare(ClassicInterval.Fifth, PureInterval.Fifth).centmap)  // List((C,-5.376572399178713), (Db,0.0), (D,-5.376572399178599), (Eb,1.
    
       println(mp.mappedStep(7)) //(Ab,696.5784284662087c)
    }
}