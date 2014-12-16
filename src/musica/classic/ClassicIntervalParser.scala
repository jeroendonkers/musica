package musica.classic
import scala.util.parsing.combinator._
import musica.symbol._
import musica.math.EitzIntervalParser

trait ClassicNoteParser extends BasicParser {
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
  
}


object ClassicNoteParser extends ClassicNoteParser {
  
  def apply(input: String): ClassicNote = parseAll(note, input) match {
  case Success(result, _) => result
  case failure : NoSuccess => ClassicNote(0)
  }
}


trait ClassicIntervalParser extends RegexParsers {
  def devname: Parser[String] = """[PMmAd]""".r
 
  def negirregular:  Parser[Int] = "i(" ~> """\d+""".r <~")" ^^ {
    case a => -(a.toInt)-100
  }
  
  def posirregular:  Parser[Int] = "I(" ~> """\d+""".r <~")" ^^ {
    case a => a.toInt+100
  }   
 
  def negsign: Parser[String] = "-"
  
  def step: Parser[Int] = """\d+""".r ^^ {
    case a => a.toInt
  } 
  
  def dev: Parser[Int] = devname ^^ {
    case c => c match {
      case "P" => 0
      case "M" => 0
      case "m" => -1
      case "A" => +1
      case "d" => -2
      
    }
  } | negirregular | posirregular
  
  
  def getdev(step: Int, dev: Int): Int = {
    if (dev< -100) dev+100
    else if (dev>100) dev-100
    else
    if (ClassicInterval.Canbepure(step % 7) & dev == -2) -1 
    else dev
  }
  
  def interval: Parser[ClassicInterval] = negsign ~ dev ~ step^^ { 
    case x ~ d ~ s  => {
      ClassicInterval(-s+1,getdev(s-1,d))
    }
  } | dev ~ step ^^ { 
    case d ~ s => ClassicInterval(s-1,getdev(s-1,d) )
  } 
 
}

object ClassicIntervalParser extends ClassicIntervalParser {
  
  def apply(input: String): ClassicInterval = parseAll(interval, input) match {
  case Success(result, _) => result
  case failure : NoSuccess => ClassicInterval(0)
  }
}



trait ClassicEventParser extends EitzIntervalParser {
  
  
    def classicnoteevent: Parser[ClassicNoteEvent] = note ~ value ~ duration ^^ {
      case n ~ t ~ d => new ClassicNoteEvent(n,t,d)
    } | note ~ duration ~ value ^^ {
      case n ~ d ~ t => new ClassicNoteEvent(n,t,d)
    }  | note ~ value ^^ {
      case n ~ t => new ClassicNoteEvent(n,t,1)
    }  | note ~ duration ^^ {
      case n ~ d => new ClassicNoteEvent(n,0,d)
    }  | note ^^ {
      case n  => new ClassicNoteEvent(n,0,1)
    }
     
    def eitzevent: Parser[EitzEvent] =  eitzpure ~ value ~ duration ^^ {
     case n ~ t ~ d => new EitzEvent(n,t,d)
    } | eitzpure ~ duration ~ value ^^ {
      case n ~ d ~ t => new EitzEvent(n,t,d)
    }  | eitzpure ~ value ^^ {
      case n ~ t => new EitzEvent(n,t,1)
    }  | eitzpure ~ duration ^^ {
      case n ~ d => new EitzEvent(n,0,d)
    }  | eitzpure ^^ {
      case n  => new EitzEvent(n,0,1)
    }
  
    def classicevent: Parser[Event] = eitzevent | classicnoteevent
 
}


object ClassicEventParser extends ClassicEventParser {
  
     
    def apply(input: String): Event = parseAll(classicevent, input) match {
    case Success(result, _) => result
    case failure : NoSuccess => new ClassicNoteEvent("C",0)
    }
    
    def eitz(input: String): Event = parseAll(eitzevent, input) match {
    case Success(result, _) => result
    case failure : NoSuccess => new EitzEvent(EitzInterval("C"),0,1)
    }
    
    def note(input: String): Event = parseAll(classicnoteevent, input) match {
    case Success(result, _) => result
    case failure : NoSuccess => new ClassicNoteEvent("C",0,1)
    }
    
    
}