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
  
   def time: Parser[SymbolicTime] = "[" ~> int ~ slash ~ int <~ "]" ^^ {
      case n ~ s ~ m =>  SymbolicTime(n,m)
   }
     
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

