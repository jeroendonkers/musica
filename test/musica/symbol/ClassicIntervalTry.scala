package musica.symbol


object main {

  def main(args: Array[String]): Unit = {
    
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
   */ 
    println( ClassicInterval("C","E##+1") )
    
    val scale = ClassicScale("P1,M2,M3,P4,P5,M6,M7")
    val b = scale.on("C#")
    println(b)
    
}
  
}