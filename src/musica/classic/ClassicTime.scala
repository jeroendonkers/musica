package musica.classic
import musica.symbol._


class ClassicNoteEvent(note: ClassicNote, value: SymbolicTime, duration: SymbolicTime = 1) extends NoteEvent[ClassicNote](note, value, duration)

class EitzEvent(val event: EitzInterval, val value: SymbolicTime, override val duration: SymbolicTime) extends Event {
  type EventType = EitzInterval
  def changeValue(t: SymbolicTime) = {
     new EitzEvent(event,t, duration)
  }
  def changeDuration(d: SymbolicTime) = {
     new EitzEvent(event,value, d)
  }
  
  
}

object EitzEvent {
  def apply(s: String, value: SymbolicTime, duration: SymbolicTime = 1) = {
    new EitzEvent(EitzInterval.pure(s), value, duration)
  }
  
}

object ClassicMetrum {
  def apply(n: Int, m: Int) =  
    new Metrum( Whole/m, n, ""+ n + "/" + m)
  
}


