package musica.symbol
import scala.util.parsing.combinator._
import musica.classic.ClassicEventParser
import musica.math._


trait BasicParser extends RegexParsers {
  
   def slash: Parser[String] = "/"
   def hat: Parser[String] = "^"   
    
   def int: Parser[Int] = """\d+""".r ^^ {
     case a => a.toInt
   } 
  
   def timecode: Parser[SymbolicTime] = """:[WHQ8624]""".r ^^ {
     case ":W" => 1\1
     case ":H" => 1\2
     case ":Q" => 1\4
     case ":8" => 1\8
     case ":6" => 1\16
     case ":2" => 1\32
     case ":4" => 1\64
   } 
   
    def timecodedot: Parser[SymbolicTime] = timecode ~ "." ^^ {
     case t ~ "." => t * 3\2
   }
   def timecodedotdot: Parser[SymbolicTime] = timecode ~ ".." ^^ {
      case t ~ ".." => t * 7\4
   }
   
   def timeexpr: Parser[SymbolicTime] = "[" ~> int ~ slash ~ int <~ "]" ^^ {
      case n ~ s ~ m =>  SymbolicTime(n,m)
   }
   
   def time: Parser[SymbolicTime] = timecodedotdot | timecodedot | timecode | timeexpr
      
   def durationcode: Parser[SymbolicTime] = 
     "_" ^^^ SymbolicTime(1\1) | 
     "-" ^^^ SymbolicTime(3\4) |
     "*" ^^^ SymbolicTime(1\2) |
     "'" ^^^ SymbolicTime(1\4)
   
     
  def duration: Parser[SymbolicTime] =  durationcode | timeexpr    
     
   def rest = "R"
     
   def restevent: Parser[Rest] = rest ~ time ^^ {
     case r ~ t =>  new Rest(t) 
   } | rest ^^ {
     case r => new Rest(0/1)
   }
   
}


trait EventListParser extends JavaTokenParsers  with BasicParser {
  
   def basicevent: Parser[Event]  // needs to be specified!
  
  
   def after(a: Event, b: Event): Event  = {
     if (b.value == 0) { 
       a match {
         case c: EventList =>   a ++ b.changeValue(c.event.last.value)
         case _ => a ++ b.changeValue(a.value)
       }
     
     } else {
       a ++ b
     }
   }
   
   def nextto(a: Event, b: Event): Event  = {
     if (b.value == 0) {
       a || b.changeValue(a.value)
     } else {
       a || b
     }
   }
   
   
   def expr : Parser[Event] = chainl1(event, 
        "," ^^^ {(a: Event, b: Event) => after(a,b)} |   
        "++" ^^^ {(a: Event, b: Event) => after(a,b)} |       
        "|" ^^^ {(a: Event, b: Event) => nextto(a,b)} 
   ) 
   
   def brexp: Parser[Event] = "("~>expr<~")" ^^ {
      case e => e
    }
   
  def event: Parser[Event] =   basicevent | restevent | brexp
  
   
  def apply(input: String): Option[EventList] = {
     
    val newinput =  input.split("\n").map(_.split("//")(0)).mkString("\n")
    
    parseAll(expr, newinput) match {
      case Success(result, _) => Some(new EventList(result.getEventList))
      case failure : NoSuccess => None
    }
  }
   
}



object EventListParser extends EventListParser with  ClassicEventParser{

  def basicevent = classicnoteevent

}


object EitzEventListParser extends EventListParser with  ClassicEventParser{

  def basicevent = eitzevent

}

