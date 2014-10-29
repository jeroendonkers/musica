package musica.symbol


object SymbolicTimeEx {
 
  def main(args: Array[String]): Unit = {
   
    println(ClassicNoteSystem.Whole)
    
    println(ClassicMetrum(6,8))
    println(ClassicMetrum(2,2))
    println(ClassicMetrum.C)
    
    
    val l = List( ClassicRest.Whole, ClassicRest.Quarter)
    
    println(HasNoteSize.add(l) * 4)
    
  }
}