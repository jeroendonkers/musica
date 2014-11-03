package musica.math
import musica.symbol._



object TuningEx {

    def main(args: Array[String]): Unit = {
      
      
      val eq = Tuning.ET12
      val agricola = Tuning("1/1,135/128,9/8,1215/1024,81/64,4/3,45/32,3/2,405/256,27/16,16/9,243/128");
      val werckmeister3 = Tuning("C^0,C#^-1,D^-1/2,Eb^0,E^-3/4,F^0,F#^-1,G^-1/4,G#^-1,A^-3/4,Bb^0,B^-3/4") 
      val architas = Tuning.fromRatios("28/27,8/7,9/8,28/27,8/7,9/8")
      
      agricola.saveXML("./data","agricola","Agricola")
      
       Tuning.loadXML("./data/agricola.Tuning_Musica_xml") match {
        case Left(t) => println("succes " + t.steplist) 
        case Right(s) => println(s)
      }
      
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
      
         
      agricola.saveXML("./data","agricola","Agricola")
        
       werckmeister3.exportScl("./data","werckmeisterIII","Werckmeister III","") 

 
    }
}