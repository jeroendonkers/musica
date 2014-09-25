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
  
  @Test def ClassicIntervalDevTest() {
    val a = new ClassicInterval(0,1)
    assertEquals(a.dev,1)        
    
    val b = new ClassicInterval(4,7)
    assertEquals(b.dev,0)
    
    val c = new ClassicInterval(1,2)
    assertEquals(c.dev,1)
    
   val d = new ClassicInterval(8,14)
    println(d.normstep)
    assertEquals(d.dev,1)
    
    //val e = new ClassicInterval(-1,-2)
    //println(e.normstep)
    //assertEquals(e.dev,-1)
    

  }
  
}
