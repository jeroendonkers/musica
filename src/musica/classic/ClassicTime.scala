package musica.classic
import musica.symbol._


object ClassicMetrum {
  def apply(n: Int, m: Int) =  
    new Metrum( Whole/m, n, ""+ n + "/" + m)

  val C = new Metrum(Quarter,4,"C")
  val AllaBreva = new Metrum(Quarter,2,"C|")
       
}


