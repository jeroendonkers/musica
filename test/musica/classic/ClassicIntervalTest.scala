package musica.classic
import org.junit.Assert._
import org.junit.Test

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
     assertEquals(-ClassicInterval.MinorThird,ClassicInterval.MajorThird - ClassicInterval.Fifth)
     assertEquals(-ClassicInterval.Fifth,(-ClassicInterval.MajorThird) - ClassicInterval.MinorThird)
     assertEquals(ClassicInterval.Fifth,(ClassicInterval.MajorThird) - (-ClassicInterval.MinorThird))
     
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
     assertEquals("Minor Thirteenth", a.name)
     assertEquals("m13", a.toString)
  }
  
    @Test def ClassicIntervalStrTest2() {
     val a = ClassicInterval(11,-1)
     assertEquals("d12", a.toString)
    }

    @Test def ClassicIntervalStrTest3() {
     val a = ClassicInterval(33,-4)
     assertEquals("Irregular (-4) Sixth 32va", a.name)
     assertEquals("i(4)34", a.toString)
    }

    @Test def ClassicIntervalStrTest4() {
     val a = ClassicInterval(-2)
     assertEquals("Major Third down", a.name)
    }
    
    @Test def ClassicIntervalNormTest1() {
     val a = ClassicInterval(11,-1).normalize()
     assertEquals("Diminished Fifth", a.name)
    }
    
    @Test def ClassicIntervalInvertTest1() {
     val a = ClassicInterval(1,0).invert()
     assertEquals("Minor Seventh", a.name)
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
    
     @Test def NoteIntervaltest1() {
      val a = ClassicNote(4)
      val b = ClassicNote(6)
      val c = a.interval(b)
      assertEquals("Major Third",c.name)
    }
     
         
     @Test def NoteIntervaltest2() {
      val a = ClassicNote(0,-1)
      val b = ClassicNote(4,1)
      val c = a.interval(b)
      assertEquals("Irregular (2) Fifth",c.name)
    }
    
     @Test def Noteenhtest1() {
      val a = ClassicNote(0,1)
      val b = ClassicNote(1,-1)
      assertTrue(a.isEnharmonic(b))
      assertFalse(a == b)
    }
  
     @Test def circelfifthTest() {
     for (i <- -12 to 12) {  
        val c = (ClassicInterval.Fifth* i).normalize
        val d=ClassicNote.FifthCircle(i)
        assertEquals(c,ClassicInterval(ClassicNote(0),d))
     } 
     }
     
     @Test def classicNoteParserTest() {
       assertEquals(ClassicNote("C"),ClassicNote(0))
       assertEquals(ClassicNote("C0"),ClassicNote(0))
        assertEquals(ClassicNote("C####"),ClassicNote(0,4))
        assertEquals(ClassicNote("D+5"),ClassicNote(1,0,5))
       assertEquals(ClassicNote("Ab-4"),ClassicNote(5,-1,-4))
       assertEquals(ClassicNote("ZZZ"),ClassicNote(0))
      }
     
      @Test def classicIntervalParserTest() {
       assertEquals(ClassicInterval("P1"),ClassicInterval(0))
        assertEquals(ClassicInterval("m3"),ClassicInterval(2,-1))
        assertEquals(ClassicInterval("A5"),ClassicInterval(4,1))
       assertEquals(ClassicInterval("i(3)6"),ClassicInterval(5,-3))
       assertEquals(ClassicInterval("-M2"),ClassicInterval(-1,0))
       assertEquals(ClassicInterval("-I(20)32"),ClassicInterval(-31,20))
        assertEquals(ClassicInterval("XXX"),ClassicInterval(0))
      }
      
      @Test def classicScaleTest() {
          val scale = ClassicScale("P1,M2,M3,P4,P5,M6,M7")
          val scale2 = ClassicScale("P1","M2","M3","P4","P5","M6","M7")
          val l: List[ClassicInterval] = List("P1","M2","M3","P4","P5","M6","M7")
          val scale3 = ClassicScale(l)
          val a = scale on "C#"
          assertEquals(a(1), ClassicNote("D#"))
        
      }
    
}
