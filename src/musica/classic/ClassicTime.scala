package musica.classic
import musica.symbol._


class ClassicVNote(note: ClassicNote, notevalue: NoteValue) extends SymbolicVNote[ClassicNote](note, notevalue)

object ClassicMetrum {
  def apply(n: Int, m: Int) =  
    new Metrum( Whole/m, n, ""+ n + "/" + m)
  
}


