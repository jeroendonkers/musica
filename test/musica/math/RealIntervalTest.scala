package musica.math

import org.junit.Assert._
import org.junit.Test
import org.junit.Before

class RealIntervalTest {
  
@Test def testcreateint() {  
  // create a interval in cents
  var a: RealInterval = 200 // implicit transformation
  assertEquals(a.cents,200, 0.0001)
}

@Test def testaddint() {
  var a: RealInterval = 200 // implicit transformation
  a = a + 100
  assertEquals(a.cents,300, 0.0001)
}

@Test def testadd() {
  var a: RealInterval = 200 // implicit transformation
  var b: RealInterval = 100 // implicit transformation
  a = a + b
  assertEquals(a.cents,300, 0.0001)
}

@Test def testequals() {
  var a: RealInterval = 200 // implicit transformation
  var b: RealInterval = 200 // implicit transformation
  assertEquals(a,b)
}

@Test def testvalue() {  
  var a: RealInterval = 1200 // 1200 cents, is an octave, value should be 2
  assertEquals(a.value,2, 0.0001)
}

@Test def testsub() {
  var a =  RealInterval(1200) 
  a = a - RealInterval.Octave  // subtracting intervals
  assertEquals(a.value,1, 0.0001)
}

@Test def testnormalize() {
  var a = (RealInterval(700) * 6).normalize // multiply and normalize
  assertEquals(a.cents,600, 0.0001)
}

@Test def testdiv() {
  var a: RealInterval = 600
  a = -(a/2) // divide and negate 
  assertEquals(a.cents,-300, 0.0001)
}

@Test def testminuni() {
  var a = RealInterval(900)
  a = -a // negate 
  assertEquals(a.cents,-900, 0.0001)
}


@Test def testtoString() {
  var a: RealInterval = 900
  assertEquals(a.toString, "900.0c")
}  

@Test def teston() {
  var a: RealInterval = 900
  assertEquals(a.on(800),1345.4342,0.001)
 }
}