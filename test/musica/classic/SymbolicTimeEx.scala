package musica.classic

import musica.symbol.HasNoteSize


object SymbolicTimeEx {
 
  def main(args: Array[String]): Unit = {
   
    println(Whole)
    
    println(ClassicMetrum(6,8))
    println(ClassicMetrum(2,2))
    println(ClassicMetrum.C)
    
    
    val l = List( WholeRest, QuarterRest)
    
    println(HasNoteSize.add(l) * 4)
    
  }
}