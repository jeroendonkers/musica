package musica.math

import org.junit.Assert._
import org.junit.Test
import org.junit.Before

class PureIntervalTest {
  
  @Test def TestCreate() {
    val a = PureInterval(5,4)
    assertEquals(a.toString,"5/4")
  }
  
  @Test def Testvalue() {
    val a = PureInterval(5,4)
    assertEquals(a.value,1.25,0.001)
  }

  @Test def Testcents() {
    val a = PureInterval(5,4)
    assertEquals(a.cents,386.3137138,0.001)
  }
  
  @Test def Testcents2() {
    val a = PureInterval(2,1)
    assertEquals(a.cents,1200,0.001)
  }
  
  @Test def TestEquals() {
    val a = PureInterval(5,4)
    val b = PureInterval(10,8)
    assertEquals(a,b)
  }
  
   @Test def TestAdd() {
    val a = PureInterval(5,4)
    val b = PureInterval(3,2)
    assertEquals(a+b,PureInterval(15,8))
   }
   
   @Test def TestSub() {
    val a = PureInterval(5,4)
    val b = PureInterval(3,2)
    assertEquals(a-b,PureInterval(10,12))
   }
   
   @Test def TestMult() {
      val a = PureInterval(5,4)
      assertEquals(a * 3,PureInterval(125,64))
      assertEquals(a * 0,PureInterval.Prime)
   }
   
   @Test def TestNormalizeDown() {
      val a = PureInterval(13,4)
      assertEquals(a.normalize,PureInterval(13,8))
   }      
   
   @Test def TestNormalizeUp() {      
      val a = PureInterval(4,13)
      assertEquals(a.normalize,PureInterval(16,13))
   }

   @Test def TestNormalizeNot() {      
      val a = PureInterval(16,13)
      assertEquals(a.normalize,PureInterval(16,13))
   }
   
}