package musica.math


object PureIntervalEx {

  def main(args: Array[String]): Unit = {
  
  
  	val a = PureInterval( 48,36 ) 
  	println(a)  //  4/3
  	println(a.value)  // 1.3333...
  	println(a.cents)  // 498.04449
 	
  	val b = (PureInterval.Fifth * 4) - (PureInterval.MajorThird + (PureInterval.Octave * 2))
  	println(b) // 81/80
  	
  	val c = PureInterval(23,18)
  	println ( c * 1.2) //509.2372... c - multiply by double
  	println ( c + 230.4) // 654.764... c - adding a cents interval, but:  	
    println ( 100 + c) // 524.366...  - implicit conversion to cents (Double)  	
  	
  	println (c on 3)  // 23/6
  	println (c on Rational(3,2)) // 23/12
  	println (c on 1.5)  // 1.916666....
  	
  	val e = PureInterval.JILimit5(4,-1)
  	println(e) // 81/80
  	
  	val f = RealInterval(c) // explicit conversion
  	println(f) //  424.364... c
  	
  	val g: PureInterval = "2/3" 
    	
  }}