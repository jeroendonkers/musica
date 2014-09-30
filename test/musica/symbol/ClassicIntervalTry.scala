package musica.symbol


object main {

  def main(args: Array[String]): Unit = {
    
    val c = ClassicNote(0,-1)
    
    for (i <- 0 to 20) {  
       val b = ClassicNote(i % 7,-2, i/7-1)
       println(""+c+"-"+b+": "+c.interval(b))
      
    }
  
}
  
}