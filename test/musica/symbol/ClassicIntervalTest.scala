package musica.symbol
import org.junit.Assert._
import org.junit.Test
import org.junit.Before

class ClassicIntervalTest {

  @Test def ClassicNoteStringTest() {
    val c = new ClassicNote(2,1,0)
    assertEquals("E#", c.toString())
    val d = new ClassicNote(8,-3,2)
    assertEquals("Cbbb+2",d.toString())
    val e = new ClassicNote(4,+2,-3)
    assertEquals("G##-3",e.toString())
  }

  @Test def ClassicNoteChrTest() {
    val c = ClassicNote(2,1,0)
    assertEquals(5, c.chr)
    val d = ClassicNote(8,-3,2)
    assertEquals(21, d.chr)
    val e = ClassicNote(4,+2,-3)
    assertEquals(-27, e.chr)
  }
  
  @Test def ClassicIntervalDevTest1() {
    val a = new ClassicInterval(0,1)
    assertEquals(1, a.size)        
  }
  
  @Test def ClassicIntervalDevTest2() {
    val b = new ClassicInterval(4)
    assertEquals(7, b.size)
  }  
   
  @Test def ClassicIntervalDevTest3() {
    val c = new ClassicInterval(1,-1)
    assertEquals(1, c.size)
  }
  
  @Test def ClassicIntervalDevTest4() {
    val d = new ClassicInterval(8,0)
    assertEquals(14, d.size)
  }
  
  @Test def ClassicIntervalDevTest5() {
    val d = new ClassicInterval(-1,0)
    assertEquals(2, d.size)
  }
  
  @Test def ClassicIntervalDevTest6() {
    val d = new ClassicInterval(-8,1)
    assertEquals(15, d.size)
  }
  
  @Test def ClassicIntervalAddTest() {
    val a = new ClassicInterval(2,0)
    val b = new ClassicInterval(2,-1)
    val c = new ClassicInterval(4,0)
    assertEquals(c, a+b)
  }
  
   @Test def ClassicIntervalAddTest1() {
     assertEquals(ClassicInterval.Fifth, ClassicInterval.MinorThird+ClassicInterval.MajorThird)
  }

  @Test def ClassicIntervalAddTest2() {
     assertEquals(ClassicInterval.Fifth, ClassicInterval.Prime+ClassicInterval.Fifth)
  }
 
   
  @Test def ClassicIntervalSubTest1() {
     assertEquals(ClassicInterval.MinorThird,ClassicInterval.Fifth-ClassicInterval.MajorThird)
  }

  @Test def ClassicIntervalMulTest1() {
     val a = ClassicInterval.MajorThird
     val b = ClassicInterval(4,1) // augmented fifth
     assertEquals(b, a*2)
  }
  
  @Test def ClassicIntervalNegTest1() {
     val a = ClassicInterval.Fifth
     val b = -ClassicInterval.MinorThird 
     val c = ClassicInterval.MajorThird
     val d = a+b
     assertEquals(c, d)
  }
  
  @Test def ClassicIntervalNegNegTest1() {
     val a = ClassicInterval(6,-1)
     val b = -a 
     val c = -b
     assertEquals(c, a)
  }
  
  
  @Test def ClassicIntervalStrTest1() {
     val a = ClassicInterval(12,-1)
     assertEquals("Minor Thirteenth", a.toString())
  }
  
    @Test def ClassicIntervalStrTest2() {
     val a = ClassicInterval(11,-1)
     assertEquals("Diminished Twelfth", a.toString())
    }

    @Test def ClassicIntervalStrTest3() {
     val a = ClassicInterval(33,-4)
     assertEquals("Irregular (-4) Sixth 32va", a.toString())
    }

    @Test def ClassicIntervalStrTest4() {
     val a = ClassicInterval(-2)
     assertEquals("Major Third down", a.toString())
    }
    
    @Test def ClassicIntervalNormTest1() {
     val a = ClassicInterval(11,-1).normalize()
     assertEquals("Diminished Fifth", a.toString())
    }
    
    @Test def ClassicIntervalInvertTest1() {
     val a = ClassicInterval(1,0).invert()
     assertEquals("Minor Seventh", a.toString())
    }
    
    @Test def NotePlusIntervaltest1() {
      val a = ClassicNote(0)
      val b = ClassicInterval(1)
      val c = a+b
      assertEquals("D",c.toString())
    }
    
    @Test def NoteMinusIntervaltest1() {
      val a = ClassicNote(4)
      val b = ClassicInterval(6)
      val c = a-b
      assertEquals("Ab-1",c.toString())
    }
    
     @Test def NoteMinusIntervaltest2() {
      val a = ClassicNote(4)
      val b = ClassicInterval(6)
      val c = a + (-b)
      assertEquals("Ab-1",c.toString())
    }
    
}
