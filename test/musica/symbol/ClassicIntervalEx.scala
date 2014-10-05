package musica.symbol


object classicIntervalTry {

  def main(args: Array[String]): Unit = {
    
    val n = ClassicNote("C#+1")
    val a: ClassicNote = "Abb"
    
    println( ClassicNote(2,-1,1) ) //  Eb+1
      
    println( "" + n + " "+ n.step +" "+ n.dev+ " " + n.octave ) //C#+1 0 1 1
      
    println( "" + a + " " + a.chr + " " + a.midicode)  // Abb 7 67
    
    println( a. isEnharmonic("G")) // true
    
    println( a == "G") // false
    
    
    println( n.normalize ) // C#

    println (ClassicNote.FifthCircle(2))  // D
    println (ClassicNote.FifthCircle(-13))  // Gbb
    
    println( a.fifth ) // -11
    
    
    val s = ClassicInterval("P5")
    val t: ClassicInterval = "m16"
      
    val u: ClassicInterval = "-A4"  
    val v: ClassicInterval = "i(12)4"  
    val w: ClassicInterval = "I(10)7"
      
    println(ClassicInterval(0))  // P1  
    println(ClassicInterval(2,-1))  // m3   
    println(ClassicInterval(7,5))  // I(5)8
    
    println( "" + s + " "+ s.step +" "+ s.dev ) // P5 4 0 
    println( "" + t + " "+ t.size +" "+ t.octaves ) // m16 25 2
    
    println(t.name) // Minor Second 16va
    
    println(ClassicInterval("M2") isEnharmonic "d3") // true
    println(ClassicInterval("M2") == "d3")           // false
    
    println(ClassicInterval("C", "E"))  // M3
    println(ClassicInterval("F+1", "C"))  //-P11
    
    println(t.normalize.name) // Minor Second
    println(u.normalize.name) // Diminished Fifth
    
    
    
    /*
    
     val c = ClassicNote(0,-1)
    
    for (i <- 0 to 20) {  
       val b = ClassicNote(i % 7,-2, i/7-1)
       println(""+c+"-"+b+": "+c.interval(b))
      
    }
   */
     
   
  /*  
    for (i <- -12 to 12) {  
      println(ClassicInterval.Fifth* i)
      val c = (ClassicInterval.Fifth* i).normalize
      println(c)
     //  println(i+" "+c.step + " "+ c.normstep + " "+ c.octaves+ " "+ c.dev)
   
      val d=ClassicNote.FifthCircle(i)
      println(d)
      println(ClassicInterval(ClassicNote(0),d))
    }
   
   */  
    
   //  ClassicScale.Minor.on(ClassicNote.FifthCircle(-3)).foreach(e => println(e))
 /*
    println(ClassicNoteParser("C#-3"))
    println(ClassicNoteParser("Ebbb"))
    println(ClassicNoteParser("A+3"))
    println(ClassicNoteParser("Z"))
    * 
    */
    
    
  //  println(ClassicIntervalParser("i(-0)12").name)
   /* 
    val a: ClassicNote = "Ab"
    val i: ClassicInterval = "P5"
    println(i.on(a))
    println (i + "m3")
    println (a + "P4")
   
   * 
    
    
    
    println( ClassicInterval("C","E##+1") )
    
    val scale = ClassicScale("P1,M2,M3,P4,P5,M6,M7")
    val b = scale.on("C#")
    println(b)
   
    
    val c = ClassicNote("Abb")
    println(c.fifth)
    println(ClassicNote.FifthCircle(-11))
    * 
    * */
  
    
}
  
}