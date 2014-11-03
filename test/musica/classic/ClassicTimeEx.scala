package musica.classic

import musica.symbol._


object ClassicTimeEx {
 
  def main(args: Array[String]): Unit = {
   
    println(Whole)
    
    println(ClassicMetrum(6,8))
    println(ClassicMetrum(2,2))
    println(MetrumC)
    
    
    val l = List( WholeRest, QuarterRest, new ClassicVNote("C",Quarter))
    
    println(new TimeSegList(l).notevalue)
    
  }
}