package musica.symbol
import musica.math.Rational
import musica.classic._
import org.junit.Assert._
import org.junit.Test
import org.junit.Before


class SymbolicTimeTest {
  
  @Test def timetest1() {
    val a = Rational(3,4)
    val t = SymbolicTime(a)
    assertEquals(t.numer,a.numer)
    assertEquals(t.denom,a.denom)
  }

  @Test def timetest2() {
    val a = SymbolicTime(3,4)
    val b = SymbolicTime(1,4)
    assertEquals(SymbolicTime(1,1), a+b)
    assertEquals(SymbolicTime(1,2), a-b)
    assertEquals(true, a>b)
    assertEquals(SymbolicTime(1,8), b/2)
    assertEquals(SymbolicTime(3,8), b.dot)
    assertEquals("[3/4]", a.toString)
  }
  
  @Test def testmetrum() {
    val m = Metrum(4,4)
    assertEquals(SymbolicTime(1,1), m.onebar)
    assertEquals(SymbolicTime(7,4), m.getBarValue(1,3))
    assertEquals(SymbolicTime(13,8), m.getBarValue(1,Rational(5,2)))
    val (a,b) = m.getNumBars(SymbolicTime(13,8))
    assertEquals(1,a)
    assertEquals(Rational(5,2),b)
  }
  
  @Test def eventtest() {
     val r1 = WholeRest
     val r2 = QuarterRest
     val c1 =  new NoteEvent[ClassicNote]("C",Quarter)
     val c2 =  new NoteEvent[ClassicNote]("D",Quarter)
     
     val event = (c1 ++ r2) || (r1 ++ c2)
     
     assertEquals(4,event.count)
     assertEquals(SymbolicTime(5,4),event.value)
     
     val fixed = event.fixAt(SymbolicTime(1,4))
     assertEquals(SymbolicTime(5,4),fixed(3).start)
     
     val empty = new EventList(List())
     assertEquals(0,empty.count)
     assertEquals(SymbolicTime.zero,empty.value)
     
     assertEquals(8, (event * 2).count)
     assertEquals(0, (event * -2).count)
     assertEquals(0, (empty * 10).count)
     
     val rep = event ** (r1 ++ r2 ++ c1)
     
     assertEquals(SymbolicTime(21,4), rep.value)
     
      val repdc = event *** (r1 ++ (r2 ++ c1))
      
     assertEquals(SymbolicTime(13,2), repdc.value)
     assertEquals(19, repdc.count)
     
     val repdcc = event *** (r1 ++ !(r2 ++ c1))
     assertEquals(15, repdcc.count)      
     assertEquals(SymbolicTime(21,4), repdcc.value)     
  }
  
  
  
}