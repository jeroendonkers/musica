package musica.classic
import musica.symbol._


class ClassicNoteEvent(note: ClassicNote, value: SymbolicTime) extends NoteEvent[ClassicNote](note, value)

class EitzEvent(val event: EitzInterval, val value: SymbolicTime) extends Event {
  type EventType = EitzInterval
  def changeValue(t: SymbolicTime) = {
     new EitzEvent(event,t)
  }
}

object EitzEvent {
  def apply(s: String, value: SymbolicTime) = {
    new EitzEvent(EitzInterval.pure(s), value)
  }
  
}

object ClassicMetrum {
  def apply(n: Int, m: Int) =  
    new Metrum( Whole/m, n, ""+ n + "/" + m)
  
}


