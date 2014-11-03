package musica.classic
import musica.math._

object ClassicTuningEx {
 

    def main(args: Array[String]): Unit = {
      
           
      val qrt = Rational(-1,4)
      val meantone = FifthTuning("Eb,S,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4")
      println(meantone.centmap)
      val sauveur = FifthTuning("Eb,S,-1/5,-1/5,-1/5,-1/5,-1/5,-1/5,-1/5,-1/5,-1/5,-1/5,-1/5")
      println(sauveur.centmap)
      println(sauveur.stepmap)
      
      val bachkellner = FifthTuning("C,P,-1/5,-1/5,-1/5,-1/5,0,-1/5,0,0,0,0,0")
      println(bachkellner.centmap)
      
      println(meantone.compare(MajorThird,PureInterval.MajorThird).centmap )
      println(sauveur.compare(MajorThird,PureInterval.MajorThird).centmap )
      
      println(meantone.frequency(0, "A",415))
      
      println(meantone.frequencies(List.range(0,12), "A",415))
      
                      
      meantone.exportHauptwerk("./data","1/4 syntonic comma meantone","1/4 meantone", "quarter_meantone",
                     "800001","1.0")
                                 
      bachkellner.exportHauptwerk("./data","Bach Kellner proposed","Kellner", "kellner",
                     "800002","1.0")
                     
      bachkellner.saveXML("./data","bachkellner","Bach Kellner proposed")     
         
       meantone.saveXML("./data","quarter_meantone","1/4 syntonic comma meantone")
       
       val newmeantone = FifthTuning.loadXML("./data/quarter_meantone.FifthTuning_Musica_xml")
       newmeantone match {
        case Left(t) => println("succes " + t.centmap) 
        case Right(s) => println(s)
      }
       

       val werckmeister3 = Tuning("C^0,C#^-1,D^-1/2,Eb^0,E^-3/4,F^0,F#^-1,G^-1/4,G#^-1,A^-3/4,Bb^0,B^-3/4") 
       val mp = new ClassicMappedTuning(werckmeister3.steplist, ChromaticScale,ClassicNote("C"),"")
       println(mp.centmap) //List((C,0.0), (Db,92.17871646099708), (D,193.15685693241744), (Eb,294.134
       println(mp.intervals(Fifth).centmap) // List((C,696.5784284662087), (Db,701.9550008653874), (D,696.578
       println(mp.compare(Fifth, PureInterval.Fifth).centmap)  // List((C,-5.376572399178713), (Db,0.0), (D,-5.376572399178599), (Eb,1.
    
       println(mp.mappedStep(7)) //(Ab,696.5784284662087c)

}
}