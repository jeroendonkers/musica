package musica.symbol

class ClassicInterval(val step: Int, val dev: Int = 0) {
  // negative step means interval downwards.
  // dev = deviation from basic size
  val normstep = step.abs % 7
  val size =  ClassicInterval.basicsize(normstep) + 12 * (step.abs/7) + dev
  val octaves = normstep/7
  val canbepure = ClassicInterval.canbepure(normstep)
  
  
  
  
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