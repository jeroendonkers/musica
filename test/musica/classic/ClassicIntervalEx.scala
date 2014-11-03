package musica.classic


object classicIntervalTry {

  def main(args: Array[String]): Unit = {
    
    
    val ll = NoteSequence("A","B")
    println(ll)
    
    val n = ClassicNote("C#+1")
    val a: ClassicNote = "Abb"
    
    println( ClassicNote(2,-1,1) ) //  Eb+1
      
    println( "" + n + " "+ n.step +" "+ n.dev+ " " + n.octave ) //C#+1 0 1 1
      
    println( "" + a + " " + a.chr + " " + a.midicode)  // Abb 7 67
    
    println( a. isEnharmonic("G")) // true
    
    println( a == "G") // false
    
    
    println( n.normalize ) // C#

    println (FifthCircle(2))  // D
    println (FifthCircle(-13))  // Gbb
    
    println( a.fifth ) // -11
    
    
    val s = ClassicInterval("P5")
    val t: ClassicInterval = "m16"
      
    val u: ClassicInterval = "-A4"  
    val v: ClassicInterval = "i(12)4"  
    val w: ClassicInterval = "I(10)7"
      
    println(ClassicInterval(0))  // P1  
    println(ClassicInterval(2,-1))  // m3   
    println(ClassicInterval(7,5))  // I(5)8
    
    println( "" + s + " "+ s.step +" "+ s.dev ) // P5 4 0 
    println( "" + t + " "+ t.size +" "+ t.octaves ) // m16 25 2
    
    println(t.name) // Minor Second 16va
    
    println(ClassicInterval("M2") isEnharmonic "d3") // true
    println(ClassicInterval("M2") == "d3")           // false
    
    println(ClassicInterval("C", "E"))  // M3
    println(ClassicInterval("F+1", "C"))  //-P11
    
    println(t.normalize.name) // Minor Second
    println(u.normalize.name) // Diminished Fifth
    
    println( s + t - u*2 ) // A26
    println( -s )// -P5
    
    println (s on "C#") // G#
    println (t below "Ab+2")  // G
    
    println (s invert) // P4
    println (t invert) // M7
    
    val scale = ClassicScale("P1,M2,M3,P4,P5,M6,M7")
    println ( scale on "C#" ) // List(C#, D#, E#, F#, G#, A#, B#)
    
    val x = scale.steplist.map(e => 1)
    println(x)
    
    val seq = scale on "C#"
    val seq2 = NoteSequence(scale,"C#")
    
    println(seq2)
    /*
    
    val scale2 = ClassicScale("P1","M2","M3","P4","P5","M6","M7")
    val l: List[ClassicInterval] = List("P1","M2","M3","P4","P5","M6","M7")
    val scale3 = ClassicScale(l)
    
    println(scale.step(-4))   // -P5
    println(scale.step(-4) on "Ab")  // Db
    
    for (i <- -4 to 1) { println( ""+i+": "+(scale.step(i)) +" "+(scale.step(i) on "Ab")) }
        // -4: -P5 Db
        // -3: -P4 Eb
        // -2: -m3 F
        // -1: -m2 G
        // 0: P1 Ab
        // 1: M2 Bb
    
   val pentatonic = ClassicScale("P1,M2,P4,P5,M6")   
   val wholenote = ClassicScale("P1,M2,M3,A4,A5,A6")
    
    println ( pentatonic on "F" ) // List(F, G, Bb, C+1, D+1)
    println ( wholenote on "C" ) // List(C, D, E, F#, G#, A#)
  
    println (ClassicScale.Major on "C")   // List(C, D, E, F, G, A, B)
    println (ClassicScale.Minor on "C")   // List(C, D, Eb, F, G, Ab, Bb)
    println (ClassicScale.HarmonicMinor on "C")  // List(C, D, Eb, F, G, Ab, B)
    println (ClassicScale.MelodicMinor on "C")  // List(C, D, Eb, F, G, A, B)
    * 
    */
}
  
}