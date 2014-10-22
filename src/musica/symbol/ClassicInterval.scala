package musica.symbol

class ClassicInterval(val step: Int, val dev: Int = 0)
  extends SymbolicInterval with SI[ClassicInterval, ClassicNote] {
  // negative step means interval downwards.
  // dev = deviation from basic size
  val normstep = step.abs % 7
  val size =  ClassicInterval.Basicsize(normstep) + 12 * (step.abs/7) + dev
  val octaves = step.abs/7
  val canbepure = ClassicInterval.Canbepure(normstep)
 
  def create(st: Int, dv: Int) = new ClassicInterval(st,dv)
  
  def normalize(): ClassicInterval = {
    if (step>=0) new ClassicInterval(normstep, dev)
    else ClassicInterval((7-normstep) % 7, -dev - (if (canbepure) 0 else 1))
  }
  
  def invert(): ClassicInterval = ClassicInterval.Octave - this.normalize
  
  override def equals(that: Any): Boolean = {
  that match {
       case that: ClassicInterval => equalTo(that)
       case _ => false
     }
   }
  
    override def toString(): String = {
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
   
   def on(l: List[ClassicNote]): List[ClassicNote] = l.map(c => this on c)
   def below(l: List[ClassicNote]): List[ClassicNote] = l.map(c => this below c)  
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
   
  val Prime = ClassicInterval(0)
  val MinorSecond = ClassicInterval(1,-1)
  val MajorSecond = ClassicInterval(1)
  val MinorThird = ClassicInterval(2,-1)
  val MajorThird = ClassicInterval(2)
  val Fourth = ClassicInterval(3)
  val Fifth = ClassicInterval(4)
  val MinorSixth = ClassicInterval(5,-1)
  val MajorSixth = ClassicInterval(5)
  val MinorSeventh = ClassicInterval(6,-1)
  val MajorSeventh = ClassicInterval(6)
  val Octave = ClassicInterval(7)
 
  
  val MajorScale = ClassicScale(Prime, MajorSecond, MajorThird, Fourth, Fifth, MajorSixth, MajorSeventh)
  val MinorScale = ClassicScale(Prime, MajorSecond, MinorThird, Fourth, Fifth, MinorSixth, MinorSeventh)
  val HarmonicMinorScale = ClassicScale(Prime, MajorSecond, MinorThird, Fourth, Fifth, MinorSixth, MajorSeventh)
  val MelodicMinorScale = ClassicScale(Prime, MajorSecond, MinorThird, Fourth, Fifth, MajorSixth, MajorSeventh)
  val ChromaticScale = ClassicScale(Prime, MinorSecond, MajorSecond, MinorThird, MajorThird,
                      Fourth, Fifth, MinorSixth, MajorSixth, MinorSeventh, MajorSeventh)
  
}


class ClassicNote(stp: Int, val dev: Int = 0, val octave: Int = 0) 
 extends SymbolicNote with SN[ClassicNote, ClassicInterval]{
  val step = if (stp<0 || stp>7) 0 else stp 
  val chr = ClassicNote.NotePos(step) + dev + octave*12
  val midicode = chr+60
  val octavesteps = 7
  def create(stp: Int, dv: Int, oct: Int) = new ClassicNote(stp,dv,oct)
  def createInterval(stp: Int, dv: Int) = new ClassicInterval(stp,dv)
  
  override def toString(): String = {
    ClassicNote.NoteName(step) + 
      (if (dev>0) ("#"*dev) else (if (dev<0) ("b" * (-dev)) else "")) +
      (if (octave>0) "+"+octave else (if (octave<0) ""+octave else ""))
  } 
  
  override def equals(that: Any): Boolean = {
  that match {
       case that: ClassicNote=> equalTo(that)
       case _ => false
     }
   }
  
  def fifth() = ClassicNote.BasicFifth(step) + dev*octavesteps
} 

object ClassicNote {
  val NoteName = Array('C', 'D', 'E', 'F', 'G', 'A', 'B')
  val NotePos = Array(0,2,4,5,7,9,11)
  val BasicFifth = Array(0,2,4,-1,1,3,5)
  
  def apply(stp: Int, dev: Int = 0, octave: Int = 0) = 
    new ClassicNote(stp,dev,octave)
  
  def apply(s: String) =  ClassicNoteParser(s)
  implicit def fromString(s: String): ClassicNote = ClassicNote(s)
  
 def FifthCircle(i: Int) = ClassicNote(0) + (ClassicInterval.Fifth * i). normalize 
}


class NoteSequence(val notelist: List[ClassicNote]) {
  val size = notelist.size
  
}

object NoteSequence {
  def apply(notes: ClassicNote*) = new NoteSequence(notes.toList)
  def apply(notes: List[ClassicNote]) = new NoteSequence(notes)
  def apply(st: String) = new NoteSequence(st.split(",").toList.map(s => ClassicNote(s)))
  def apply(scale: ClassicScale, note: ClassicNote) = new NoteSequence(scale.steplist.map(e => e.on(note)))
}

class ClassicScale(val steplist: List[ClassicInterval]) {
   
   def this(stps: ClassicInterval*) { this(stps.toList) }
   def on(c: ClassicNote) = NoteSequence(this,c)
   def step(i: Int): ClassicInterval = {
     if (size==0) ClassicInterval(0) else {
       val octave = if (i>=0) (i / size) else  ((i+1) / size) - 1  
       val idx = if (i>=0) (i % size) else (size-1- ((-i-1) % size))    
       (ClassicInterval(7) * octave) + steplist(idx)
     }  
   }
   
   def stepNumber(c: ClassicInterval): Int = {
     steplist.indexOf(c)
   }
   val size = steplist.size
}

object ClassicScale {
  def apply(steps: ClassicInterval*) = new ClassicScale(steps.toList)
  def apply(steps: List[ClassicInterval]) = new ClassicScale(steps)
  def apply(st: String) = new ClassicScale(st.split(",").toList.map(s => ClassicInterval(s)))
  
  def apply(notes: NoteSequence) = new ClassicScale(
         if (notes.size==0) List() else
         notes.notelist.map(a => ClassicInterval(notes.notelist(0),a)))
  
  def Major = ClassicInterval.MajorScale 
  def Minor = ClassicInterval.MinorScale
  def HarmonicMinor = ClassicInterval.HarmonicMinorScale
  def MelodicMinor = ClassicInterval.MelodicMinorScale
  def Chromatic = ClassicInterval.ChromaticScale
  
}