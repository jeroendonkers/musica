package musica.math
import scala.util.parsing.combinator._
import musica.symbol.ClassicNote

object RealIntervalParser  extends RegexParsers {
  
  
  // Eitz notation
  
   def notename: Parser[Int] = """[A-G]""".r ^^ {
    case a => ((a.charAt(0).toInt - 'A'.toInt) + 5) % 7 
  }
  
  def sharp: Parser[Int] = "#+".r ^^ {
    case a => a.length
  }
 
  def flat: Parser[Int] = "b+".r ^^ {
    case a => -(a.length)
  }
  
  def octave: Parser[Int] = """\+\d+""".r ^^ {
    case a => a.toInt
  }  | """-\d+""".r ^^ {
    case a => a.toInt
  }  | "0" ^^ { case _ => 0 }
  
  def note: Parser[ClassicNote] = notename ~ (sharp | flat) ~ octave^^ { 
    case a ~ b ~ c  => ClassicNote(a,b,c)
  } | notename ~ (sharp | flat) ^^ { 
    case a ~ b => ClassicNote(a,b)
  } | notename ~  octave^^ { 
    case a ~ c  => ClassicNote(a,0,c)
  } | notename ^^ { 
    case a  => ClassicNote(a)
  }
  
  
  def slash: Parser[String] = "/"
    
  def int: Parser[Int] = """\d+""".r ^^ {
    case a => a.toInt
  } 
  
  
    def eitzpure: Parser[PureInterval] =  note  ^^ { 
    case n  => EitzInterval(n.toString())
    } 
  
    def eitz: Parser[RealInterval] = note ~ slash ~ int ^^ { 
    case n ~ s ~ t => EitzInterval(n.normalize().toString(), 1.0 * n.octave / t)
    } | eitzpure
  
    def ratio: Parser[PureInterval] = int ~ slash ~ int  ^^ {
      case n ~ s ~ m =>  PureInterval(n,m)
    }
    
    def c: Parser[String] = "c"
    def double: Parser[Double] = """\d+.\d+""".r ^^ { 
      case d => d.toDouble
    }  
      
    def cents: Parser[RealInterval] = double ~ c ^^ {
      case a ~ c => CentsInterval(a)
    } | int ~ c ^^ {
      case a ~ c => CentsInterval(a)
    }
    
    def interval: Parser[RealInterval] = eitz | ratio | cents
    
    def apply(input: String): RealInterval = parseAll(interval, input) match {
    case Success(result, _) => result
    case failure : NoSuccess => RealInterval(1)
    }
    
    
    
    
    def pure: Parser[PureInterval] = eitzpure | ratio
    
    def pure(input: String): PureInterval = parseAll(pure, input) match {
    case Success(result, _) => result
    case failure : NoSuccess => PureInterval(1,1)
    }
}