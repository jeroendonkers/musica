package musica.symbol

class ClassicInterval(val step: Int, val size: Int) {
  val normstep = step % 7 + (if (step<0) 7 else 0)
  val dev = (if (!ClassicInterval.canbepure(normstep)) 1 else 0) +
  size - ClassicInterval.basicsize(normstep) - 12 * (step/7) +
  (if (step<0) 12 else 0)
    
}

object ClassicInterval {
  val intervalName =  Array ("Prime", "Second","Third", "Fourth", 
      "Fifth", "Sixth", "Seventh", "Octave", "Ninth")
  val canbepure = Array(true, false, false, true, true, false, false)
  val basicsize = Array(0,2,4,5,7,9,11)
}

class ClassicNote(stp: Int, val dev: Int, val octave: Int = 0) {
  val step = if (stp<0 || stp>7) 0 else stp 
  val chr = ClassicNote.NotePos(step) + dev + octave*12
  override def toString(): String = {
    ClassicNote.NoteName(step) + 
      (if (dev>0) ("#"*dev) else (if (dev<0) ("b" * (-dev)) else "")) +
      (if (octave>0) "+"+octave else (if (octave<0) ""+octave else ""))
  } 
}

object ClassicNote {
  val NoteName = Array('C', 'D', 'E', 'F', 'G', 'A', 'B')
  val NotePos = Array(0,2,4,5,7,9,11)
  
  def apply(stp: Int, dev: Int, octave: Int = 0) = 
    new ClassicNote(stp,dev,octave)
  
}