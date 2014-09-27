package musica.symbol
import org.junit.Assert._
import org.junit.Test
import org.junit.Before

class ClassicIntervalTest {

  @Test def ClassicNoteStringTest() {
    val c = new ClassicNote(2,1,0)
    assertEquals(c.toString(), "E#")
    val d = new ClassicNote(8,-3,2)
    assertEquals(d.toString(), "Cbbb+2")
    val e = new ClassicNote(4,+2,-3)
    assertEquals(e.toString(), "G##-3")
  }

  @Test def ClassicNoteChrTest() {
    val c = ClassicNote(2,1,0)
    assertEquals(c.chr, 5)
    val d = ClassicNote(8,-3,2)
    assertEquals(d.chr, 21)
    val e = ClassicNote(4,+2,-3)
    assertEquals(e.chr, -27)
  }
  
  @Test def ClassicIntervalDevTest1() {
    val a = new ClassicInterval(0,1)
    assertEquals(a.size,1)        
  }
  
  @Test def ClassicIntervalDevTest2() {
    val b = new ClassicInterval(4)
    assertEquals(b.size,7)
  }  
   
  @Test def ClassicIntervalDevTest3() {
    val c = new ClassicInterval(1,-1)
    assertEquals(c.size,1)
  }
  
  @Test def ClassicIntervalDevTest4() {
    val d = new ClassicInterval(8,0)
    assertEquals(d.size,14)
  }
  
  @Test def ClassicIntervalDevTest5() {
    val d = new ClassicInterval(-1,0)
    assertEquals(d.size,2)
  }
  
  @Test def ClassicIntervalDevTest6() {
    val d = new ClassicInterval(-8,1)
    assertEquals(d.size,15)
  }
    
  
}
