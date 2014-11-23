package musica.symbol
import scala.util.parsing.combinator._
import musica.classic.ClassicEventParser

trait EventParser extends RegexParsers {
  
   def event: Parser[Event]  // needs to be specified!
  
   def chainevents: Parser[EventList]= "("~>repsep(event,",")<~")" ^^ {
      case ss => new EventList(ss)
    }
   
}



object EventParser extends EventParser with  ClassicEventParser{

  
   
   
  def apply(input: String): Option[EventList] = {

    
     
  parseAll(chainevents, input) match {
  case Success(result, _) => Some(result)
  case failure : NoSuccess => None
  }
  }
}