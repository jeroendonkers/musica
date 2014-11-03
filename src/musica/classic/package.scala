package musica
import musica.symbol._

package object classic {

   def FifthCircle(i: Int) = ClassicNote(0) + (Fifth * i). normalize 
  
  // classic intervals
  
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
  
  // scales
  
  val MajorScale = ClassicScale(Prime, MajorSecond, MajorThird, Fourth, Fifth, MajorSixth, MajorSeventh)
  val MinorScale = ClassicScale(Prime, MajorSecond, MinorThird, Fourth, Fifth, MinorSixth, MinorSeventh)
  val HarmonicMinorScale = ClassicScale(Prime, MajorSecond, MinorThird, Fourth, Fifth, MinorSixth, MajorSeventh)
  val MelodicMinorScale = ClassicScale(Prime, MajorSecond, MinorThird, Fourth, Fifth, MajorSixth, MajorSeventh)
  val ChromaticScale = ClassicScale(Prime, MinorSecond, MajorSecond, MinorThird, MajorThird,
                      Fourth, Fifth, MinorSixth, MajorSixth, MinorSeventh, MajorSeventh)
  
  // time
  
  val Whole = new NoteValue(1,1)
  val Half = Whole / 2
  val Quarter = Half / 2
  val Eighth = Quarter / 2
  val Sixteenth = Eighth / 2
  val ThirtySecondth = Sixteenth / 2
  val SixtyFourth = ThirtySecondth / 2
  val DoubleWhole = Whole * 2
  val QuadrupleWhole = Whole * 4
  val OctupleWhole = Whole * 8
  
  val Maxima = OctupleWhole
  val Longa = QuadrupleWhole
  val Breve = DoubleWhole
  val Semibreve = Whole
  val Minim = Half
  val Crotchet = Quarter
  val Quaver = Eighth
  val Semiquaver = Sixteenth
  val Demisemiquaver = ThirtySecondth
  val Hemidemisemiquaver = SixtyFourth
  
  // rests
  
  val DoubleWholeRest = new Rest(DoubleWhole)
  val WholeRest = new Rest(Whole)
  val HalfRest = new Rest(Half)
  val QuarterRest = new Rest(Quarter)
  val EighthRest = new Rest(Eighth)
  val SixteenthRest = new Rest(Sixteenth)
  val ThirtySecondthRest = new Rest(ThirtySecondth)
  val SixtyFourthRest = new Rest( SixtyFourth)
  
  // metrum
  
  val MetrumC = new Metrum(Quarter,4,"C")
  val MetrumAllaBreva = new Metrum(Quarter,2,"C|")
}