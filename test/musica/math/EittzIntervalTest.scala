package musica.math

import org.junit.Assert._
import org.junit.Test
import org.junit.Before


class EittzIntervalTest {

    @Test def TestCreate1() {
    val a = EitzInterval("C")
    assertEquals(a.toString,"C^0")
  }
  
}