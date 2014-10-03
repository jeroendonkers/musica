package musica.symbol


object main {

  def main(args: Array[String]): Unit = {
    
   /* val c = ClassicNote(0,-1)
    
    for (i <- 0 to 20) {  
       val b = ClassicNote(i % 7,-2, i/7-1)
       println(""+c+"-"+b+": "+c.interval(b))
      
    }
    */
     
   
    /*
    for (i <- -12 to 12) {  
     // println(ClassicInterval.Fifth* i)
      val c = (ClassicInterval.Fifth* i).normalize
      println(c)
     //  println(i+" "+c.step + " "+ c.normstep + " "+ c.octaves+ " "+ c.dev)
   
      val d=ClassicNote.FifthCircle(i)
      println(d)
      println(ClassicInterval(ClassicNote(0),d))
    }
    */
    
     ClassicScale.Minor.on(ClassicNote.FifthCircle(-3)).foreach(e => println(e))
  
}
  
}