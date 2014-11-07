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
   val longvalue: Long = numer / denom
   
   

   def +(that: Rational) = new Rational(numer * that.denom + that.numer * denom,denom * that.denom)
   def -(that: Rational) = new Rational(numer * that.denom - that.numer * denom,denom * that.denom)
   def *(that: Rational) = new Rational(numer * that.numer, denom * that.denom)
   def /(that: Rational) = new Rational(numer * that.denom, denom * that.numer)
   def unary_- :Rational = new Rational(-numer, denom)
   
   def negative: Boolean = numer < 0
   
   def toInt = longvalue.toInt
   def toILong = longvalue
   def toDouble = value
   def toFloat = value.toFloat
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
  
  val zero = Rational(0,1)
  
implicit object RationalNumeric extends Numeric[Rational]  {
   // implementing Numeric trait
    def compare(x: Rational, y: Rational): Int = {
      if (x.value < y.value) -1 else +1
    }
    def fromInt(x: Int): Rational = Rational(x,1)
    def minus(x: Rational, y: Rational): Rational = x - y
    def negate(x: Rational): Rational = -x
    def plus(x: Rational, y: Rational): Rational = x + y
    def times(x: Rational, y: Rational): Rational = x*y
    def toDouble(x: Rational): Double = x.toDouble
    def toFloat(x: Rational): Float = x.toFloat
    def toInt(x: Rational): Int = x.toInt
    def toLong(x: Rational): Long = x.toLong
   
   
}    
}