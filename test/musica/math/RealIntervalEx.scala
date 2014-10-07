package musica.math

object RealIntervalEx {

  def main(args: Array[String]): Unit = {
  
  
  	val a = CentsInterval( 200.0 ) 
 	val b = RealInterval( 1100 )  //  creates a CentsInterval
	val c: RealInterval  = 1000.0  // implicit conversion 
	val y: RealInterval = "1000.0c" // another conversion (mind the "c"!)

	val d = (a + b) * 3 - (c / 2.5) + 230.0
	
	println (d.value)  // 0.139837...
  	println (d.cents)  // 3630.0
  	
  	val e = d.normalize
  	
  	println (e.value)  // 0.139837...
  	println (e.cents)  // 3630.0
  	
  	println(d on 1.5) // 12.2097...
  	
  	println(100 + d)  // 3830.0 - implicit conversion to cents
  	
  }}