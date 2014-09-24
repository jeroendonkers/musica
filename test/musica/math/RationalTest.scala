package musica.math
import org.junit.Assert._
import org.junit.Test
import org.junit.Before

class RationalTest {
  @Test def testvalue() {
    var a = Rational(3,4)
    assertEquals(a.value,0.75, 0.001)
  }
  
  @Test def testequals() {
    var a = Rational(3,4)
    var b = Rational(6,8)
    assertEquals(a,b)
    assertTrue(a == b)
  }
  
@Test def testplusmin() {
    var a = Rational(3,4)
    var b = Rational(4,5)
    assertTrue(a + b == Rational(31,20))
    assertTrue(b - a == Rational(1,20))
  }

@Test def testtimesdiv() {
    var a = Rational(3,4)
    var b = Rational(4,5)
    assertTrue(a * b == Rational(12,20))
    assertTrue(a / b == Rational(15,16))
  }

@Test def teststring() {
    var a = Rational(3,4)
    assertEquals(a.toString,"3/4")
  }
}

