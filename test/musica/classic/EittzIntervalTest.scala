package musica.classic

import org.junit.Assert._
import org.junit.Test
import musica.classic._
import musica.math._



class EittzIntervalTest {

    @Test def TestCreate1() {
    val a = EitzInterval("C")
    assertEquals(a.toString,"C^0")
    } 
    
    @Test def TestCreate2() {
    val a = EitzInterval("C",1)
    assertEquals(a.toString,"C^+1")
    }
  
    @Test def TestCreate3() {
    val a = EitzInterval("C",Rational(1,2))
    assertEquals(a.toString,"C^+1/2")
    }
    
    @Test def TestCreate4() {
    val a = EitzInterval("C^+1/2")
    assertEquals(a.toString,"C^+1/2")
    }
    
    @Test def TestCreate5() {
    val a = EitzInterval.pure("C^+2")
    assertEquals(a.toString,"C^+2")
    
    val b = EitzInterval.pure("C^+2/3")
    assertEquals(b.toString,"C^0")
    
    val c = EitzInterval("C^+2/3")
    assertEquals(c.toString,"C^+2/3")
    
    }
    
    @Test def Test2() {
    val a = EitzInterval.pure("E^-1")
    assertEquals(a * 1,PureInterval("5/4") * 1)
    }
    
    
    
}