package musica.symbol

class ClassicInterval(val step: Int, val dev: Int = 0) {
  // negative step means interval downwards.
  // dev = deviation from basic size
  val normstep = step.abs % 7
  val size =  ClassicInterval.Basicsize(normstep) + 12 * (step.abs/7) + dev
  val octaves = step.abs/7
  val canbepure = ClassicInterval.Canbepure(normstep)
  
   def normalize(): ClassicInterval = {
    if (step>=0) new ClassicInterval(normstep, dev)
    else ClassicInterval((7-normstep) % 7, -dev - (if (canbepure) 0 else 1))
  }
    
  def +(that: ClassicInterval): ClassicInterval = if (that.step<0) this - -that else
     new ClassicInterval(step + that.step, size + that.size - ClassicInterval(step + that.step).size)
  def -(that: ClassicInterval): ClassicInterval = if (that.step<0) this + -that else
     new ClassicInterval(step - that.step, size - that.size - ClassicInterval(step - that.step).size)
  def *(that: Int) = 
     new ClassicInterval(step * that, size * that.abs - ClassicInterval(step * that).size)
  def unary_- :ClassicInterval = ClassicInterval(-step,dev)
  
  def invert(): ClassicInterval = ClassicInterval.Octave - this
  
  override def equals(that: Any): Boolean = {
  that match {
       case that: ClassicInterval => (this.step == that.step) && (this.dev == that.dev)
       case _ => false
     }
   }
  
   override def toString(): String = {
     val basname = if (step.abs<15) ClassicInterval.IntervalName(step.abs) else
                        ClassicInterval.IntervalName(normstep)+" "+(octaves*8)+"va"
     val aspect = if (ClassicInterval.Canbepure(normstep)) dev match {
       case 0 => "P" 
       case 1 => "A"
       case -1 => "d" 
       case _ => if (dev<0) "i("+(-dev)+")" else "I("+dev+")"  
     } else dev match {
       case 0 => "M" 
       case 1 => "A"
       case -1 => "m"
       case -2 => "d"
       case _ => if (dev<0) "i("+(-dev)+")" else "I("+dev+")"
     }
     val sign = if (step<0) "-" else ""
     sign + aspect + (step.abs+1)                
   }
   
   
    def name(): String = {
     val basname = if (step.abs<15) ClassicInterval.IntervalName(step.abs) else
                        ClassicInterval.IntervalName(normstep)+" "+(octaves*8)+"va"
     val aspect = if (ClassicInterval.Canbepure(normstep)) dev match {
       case 0 => "Pure" 
       case 1 => "Augmented"
       case -1 => "Diminished" 
       case _ => "Irregular ("+dev+")"  
     } else dev match {
       case 0 => "Major" 
       case 1 => "Augmented"
       case -1 => "Minor"
       case -2 => "Diminished"
       case _ => "Irregular ("+dev+")"  
     }
     val sign = if (step<0) " down" else ""
     aspect + " "+ basname + sign                  
   }
   
   
   
   def on(c: ClassicNote): ClassicNote = c + this
   def below(c: ClassicNote): ClassicNote = c - this
   
}

object ClassicInterval {
  val IntervalName =  Array ("Unison", "Second","Third", "Fourth", 
      "Fifth", "Sixth", "Seventh", "Octave", "Ninth", "Tenth", "Eleventh", "Twelfth", "Thirteenth", "Fourteenth", "Fifteenth")
  
  val Canbepure = Array(true, false, false, true, true, false, false)
  val Basicsize = Array(0,2,4,5,7,9,11)
  
  def apply(step: Int, dev: Int = 0) = new ClassicInterval(step,dev)
  def apply(note1: ClassicNote, note2: ClassicNote) = note1.interval(note2)
  def apply(s: String) =  ClassicIntervalParser(s)
  implicit def fromString(s: String): ClassicInterval = ClassicInterval(s)
  
   
  def Prime = ClassicInterval(0)
  def MinorSecond = ClassicInterval(1,-1)
  def MajorSecond = ClassicInterval(1)
  def MinorThird = ClassicInterval(2,-1)
  def MajorThird = ClassicInterval(2)
  def Fourth = ClassicInterval(3)
  def Fifth = ClassicInterval(4)
  def MinorSixth = ClassicInterval(5,-1)
  def MajorSixth = ClassicInterval(5)
  def MinorSeventh = ClassicInterval(6,-1)
  def MajorSeventh = ClassicInterval(6)
  def Octave = ClassicInterval(7)
 
  
  def MajorScale = ClassicScale(List(Prime, MajorSecond, MajorThird, Fourth, Fifth, MajorSixth, MajorSeventh))
  def MinorScale = ClassicScale(List(Prime, MajorSecond, MinorThird, Fourth, Fifth, MinorSixth, MinorSeventh))
  def HarmonicMinorScale = ClassicScale(List(Prime, MajorSecond, MinorThird, Fourth, Fifth, MinorSixth, MajorSeventh))
  def MelodicMinorScale = ClassicScale(List(Prime, MajorSecond, MinorThird, Fourth, Fifth, MajorSixth, MajorSeventh))
  
}



class ClassicNote(stp: Int, val dev: Int = 0, val octave: Int = 0) {
  val step = if (stp<0 || stp>7) 0 else stp 
  val chr = ClassicNote.NotePos(step) + dev + octave*12
  val midicode = chr+60
  
  def +(that: ClassicInterval): ClassicNote = {
     if (that.step <0) this - (-that) else {   
    
     val newstep = (step + that.step) % 7
     val newoctave = octave + (step + that.step) / 7
     
     val newdev = chr + that.size - ClassicNote(newstep,0,newoctave).chr
     new ClassicNote(newstep,newdev, newoctave)
     }
  }
  
  def -(that: ClassicInterval): ClassicNote =  {
    if (that.step <0) this + (-that) else {
      
      val ns = that.step % 7
      val newstep = (step - ns + (if (ns>step) 7 else 0)) % 7
      val newoctave = octave + (step - that.step) / 7 - (if (that.step>step) 1 else 0)
      val newdev = if (that.step % 7 ==0) (dev-that.dev) else ((chr - that.size) - (ClassicNote(newstep,0,newoctave).chr))
      new ClassicNote(newstep,newdev, newoctave)
     }
  }
  
  override def toString(): String = {
    ClassicNote.NoteName(step) + 
      (if (dev>0) ("#"*dev) else (if (dev<0) ("b" * (-dev)) else "")) +
      (if (octave>0) "+"+octave else (if (octave<0) ""+octave else ""))
  } 
  
  def interval(that: ClassicNote): ClassicInterval = {
    val newstep = that.step + that.octave*7 - step - octave*7
    val chrdiv = that.chr - chr
    val bassize = ClassicInterval(newstep).size 
    val dev = (if (newstep==0) chrdiv else chrdiv.abs) - bassize
    ClassicInterval(newstep, dev)
  }
  
  override def equals(that: Any): Boolean = {
  that match {
       case that: ClassicNote=> (this.step == that.step) && (this.dev == that.dev) && (this.octave == that.octave)
       case _ => false
     }
   }
  
  def isEnharmonic(that: ClassicNote): Boolean = this.chr == that.chr

  def normalize() = new ClassicNote(step,dev,0)
}

object ClassicNote {
  val NoteName = Array('C', 'D', 'E', 'F', 'G', 'A', 'B')
  val NotePos = Array(0,2,4,5,7,9,11)
  
  def apply(stp: Int, dev: Int = 0, octave: Int = 0) = 
    new ClassicNote(stp,dev,octave)
  
  def apply(s: String) =  ClassicNoteParser(s)
  implicit def fromString(s: String): ClassicNote = ClassicNote(s)
  
 def FifthCircle(i: Int) = ClassicNote(0) + (ClassicInterval.Fifth * i). normalize 
}

class ClassicScale(val step: List[ClassicInterval]) {
   def on(c: ClassicNote) = step.map(e => e.on(c))
}

object ClassicScale {
  def apply(step: List[ClassicInterval]) = new ClassicScale(step)
  
  def apply(st: String) = new ClassicScale(st.split(",").toList.map(s => ClassicInterval(s)))
  
  def Major = ClassicInterval.MajorScale 
  def Minor = ClassicInterval.MinorScale
  def HarmonicMinor = ClassicInterval.HarmonicMinorScale
  def MelodicMinor = ClassicInterval.MelodicMinorScale
  
}