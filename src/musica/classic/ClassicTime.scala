package musica.classic
import musica.symbol._


class ClassicNoteEvent(note: ClassicNote, value: NoteValue) extends NoteEvent[ClassicNote](note, value)

object ClassicMetrum {
  def apply(n: Int, m: Int) =  
    new Metrum( Whole/m, n, ""+ n + "/" + m)
  
}


