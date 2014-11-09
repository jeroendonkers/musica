package musica.classic

import musica.symbol._


class ClassicInterval(step: Int, dev: Int=0)
  extends SymbolicInterval[ClassicInterval, ClassicNote](step, dev,7,12) {
  // negative step means interval downwards.
  // dev = deviation from basic size
    
  def canbepure(st: Int) = ClassicInterval.Canbepure(st)
  def basicsize(st: Int) = ClassicInterval.Basicsize(st)
  def create(st: Int, dv: Int) = new ClassicInterval(st,dv)
  
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
  
}


class ClassicNote(stp: Int, dev: Int = 0, octave: Int = 0) 
 extends SymbolicNote[ClassicNote, ClassicInterval](stp, dev, octave,7,12) with HasMidiCode {
 
  def create(stp: Int, dv: Int, oct: Int) = new ClassicNote(stp,dv,oct)
  def createInterval(stp: Int, dv: Int) = new ClassicInterval(stp,dv)
  
  def notepos(st: Int) = ClassicNote.NotePos(st) 
  val midicode = chr+60
  
  
  override def toString(): String = {
    ClassicNote.NoteName(step % 7) + 
      (if (dev>0) ("#"*dev) else (if (dev<0) ("b" * (-dev)) else "")) +
      (if (octave>0) "+"+octave else (if (octave<0) ""+octave else ""))
  } 
  
 
  def fifth() = ClassicNote.BasicFifth(step % 7) + dev*octavesteps
} 

object ClassicNote {
  val NoteName = Array('C', 'D', 'E', 'F', 'G', 'A', 'B')
  val NotePos = Array(0,2,4,5,7,9,11)
  val BasicFifth = Array(0,2,4,-1,1,3,5)
  
  def apply(stp: Int, dev: Int = 0, octave: Int = 0) = 
    new ClassicNote(stp,dev,octave)
  
  def apply(s: String) =  ClassicNoteParser(s)
  implicit def fromString(s: String): ClassicNote = ClassicNote(s)
  
}


object NoteSequence {
  def apply(notes: ClassicNote*) = notes.toList
  def apply(notes: List[ClassicNote]) = notes
  def apply(st: String) = st.split(",").toList.map(ClassicNote(_))
  def apply(scale: ClassicScale, note: ClassicNote) = scale.steplist.map(_.on(note))
}

class ClassicScale(steplist: List[ClassicInterval]) 
  extends SymbolicScale[ClassicNote,ClassicInterval](steplist){
   val prime = ClassicInterval(0)
   val octave = ClassicInterval(7)
}

object ClassicScale {
  def apply(steps: ClassicInterval*) = new ClassicScale(steps.toList)
  def apply(steps: List[ClassicInterval]) = new ClassicScale(steps)
  def apply(st: String) = new ClassicScale(st.split(",").toList.map(ClassicInterval(_)))
  
  def fromNotes(notes: List[ClassicNote]) = new ClassicScale(
         if (notes.size==0) List() else
         notes.map(ClassicInterval(notes(0),_)))
  
  
}