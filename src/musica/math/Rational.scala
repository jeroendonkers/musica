package musica.math

class Rational(n: Long, d: Long) {
   private def gcd(x: Long, y: Long): Long = {
	if (x == 0) y
    else if (x < 0) gcd(x,y)
    else if (y < 0) gcd(x, y)
    else gcd(y % x, x)
    }
   
   private val g = if(n!=0 && d!=0) gcd(n.abs, d.abs) else 1
   val numer: Long = if (n==0 || d==0) 0 else n/g
   val denom: Long = if (n==0 || d==0) 1 else d/g
   val value: Double = 1.0 * numer/denom;

   def +(that: Rational) = new Rational(numer * that.denom + that.numer * denom,denom * that.denom)
   def -(that: Rational) = new Rational(numer * that.denom - that.numer * denom,denom * that.denom)
   def *(that: Rational) = new Rational(numer * that.numer, denom * that.denom)
   def /(that: Rational) = new Rational(numer * that.denom, denom * that.numer)
   
   override def toString = "" + numer + "/" + denom
   
   override def equals(that: Any): Boolean = {
     that match {
       case that: Rational => this.numer == that.numer && this.denom == that.denom
       case _ => false
     }
   }
   
   
}

object Rational {
  def apply(n: Int, d: Int) = new Rational(n,d) 
    
}