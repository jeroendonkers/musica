package musica.classic
import musica.symbol._


class ClassicNoteEvent(note: ClassicNote, value: SymbolicTime) extends NoteEvent[ClassicNote](note, value)

class EitzEvent(val event: EitzInterval, val value: SymbolicTime) extends Event {
  type EventType = EitzInterval
}

object ClassicMetrum {
  def apply(n: Int, m: Int) =  
    new Metrum( Whole/m, n, ""+ n + "/" + m)
  
}


