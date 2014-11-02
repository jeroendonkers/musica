package musica.classic

import musica.math._
import musica.classic._



object EitzIntervalEx {

    def main(args: Array[String]): Unit = {
      
    println(EitzInterval("C^0")) // 1/1
    println(EitzInterval("C^0").isPure) // 1/1
    
  	println(EitzInterval("E^+1")) // 5/4 = pure third
  	println(PureInterval(EitzInterval.pure("E^+1")))
  	println(EitzInterval("E^-1")) // 5/4 = pure third
  	println(PureInterval(EitzInterval.pure("E^-1")))
  	
  	println(EitzInterval("E^-1") == PureInterval(5,4))
  	
  	println(EitzInterval("E+1^+1")) // 6561/5120
  	println(EitzInterval("Bb+1")) // 9/5
  	println(EitzInterval("F##^+1/4")) 
  	println(EitzInterval("F##^+1/4").isPure) 	
  	
  	println(EitzInterval("E+1", -1)) // 5/2 = pure third + octave
  	println(EitzInterval("E", -1)) // 5/4 = pure third
  	
  	
  	println(EitzInterval("D+1", Rational(-1,7))) // third comma: 196.741... c
  	println(EitzInterval("A+1", 1,2).cents)
    println(EitzInterval("A").cents)

    }
  
}