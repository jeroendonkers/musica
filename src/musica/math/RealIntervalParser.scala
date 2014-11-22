package musica.math
import scala.util.parsing.combinator._
import musica.classic.ClassicNote
import musica.classic.PureEitzInterval
import musica.classic.EitzInterval
import musica.classic.ClassicNoteParser

object RealIntervalParser  extends RegexParsers {

   // borrow octave and note parsers from ClassicNoteParser
  
  def octave: Parser[Int] = ClassicNoteParser.octave.asInstanceOf[Parser[Int]]
  
  def note: Parser[ClassicNote] = ClassicNoteParser.note.asInstanceOf[Parser[ClassicNote]]
    
  def slash: Parser[String] = "/"
  def hat: Parser[String] = "^"   
    
    
  def int: Parser[Int] = """\d+""".r ^^ {
    case a => a.toInt
  } 
  
  
    def eitzpureimpl: Parser[PureEitzInterval] =  note  ^^ { 
    case n  => EitzInterval(n)
    } 
    
    def eitzpure: Parser[PureEitzInterval] =  note ~ hat ~ octave  ^^ { 
    case n ~ h ~ i => EitzInterval(n,i)
    } | eitzpureimpl
     
  
    def eitz: Parser[RealInterval] = note ~ hat ~ octave ~ slash ~ int ^^ { 
    case n ~ h ~ u ~s ~ t => EitzInterval(n, Rational(u, t))
    } | eitzpure | eitzpureimpl
  
    def ratio: Parser[PureInterval] = int ~ slash ~ int  ^^ {
      case n ~ s ~ m =>  PureInterval(n,m)
    }
    
    def sratio: Parser[Rational] = octave ~ slash ~ int  ^^ {
      case n ~ s ~ m =>  Rational(n,m)
    } | octave ^^ {
      case n => Rational(n,1)
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
    
    def pure: Parser[PureInterval] = eitzpure | ratio
        
    def apply(input: String): RealInterval = parseAll(interval, input) match {
    case Success(result, _) => result
    case failure : NoSuccess => RealInterval(1)
    }
    
    def eitz(input: String): RealInterval = parseAll(eitz, input) match {
    case Success(result, _) => result
    case failure : NoSuccess => PureInterval(1,1)
    }   
    
    def pureEitz(input: String): PureEitzInterval = parseAll(eitzpure, input) match {
    case Success(result, _) => result
    case failure : NoSuccess => EitzInterval("C")
    }   
    
    def pure(input: String): PureInterval = parseAll(pure, input) match {
    case Success(result, _) => result
    case failure : NoSuccess => PureInterval(1,1)
    }
    
    def aratio(input: String): Rational = parseAll(sratio, input) match {
    case Success(result, _) => result
    case failure : NoSuccess => Rational(0,1)
    }
}