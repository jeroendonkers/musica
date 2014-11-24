package musica.symbol
import scala.util.parsing.combinator._
import musica.classic.ClassicEventParser

trait EventListParser extends JavaTokenParsers {
  
   def basicevent: Parser[Event]  // needs to be specified!
  
  
   def expr : Parser[Event] = chainl1(event, 
        "," ^^^ {(a: Event, b: Event) => a ++ b} |   
        "++" ^^^ {(a: Event, b: Event) => a ++ b} |       
        "|" ^^^ {(a: Event, b: Event) => a || b} 
   ) 
   
   def brexp: Parser[Event] = "("~>expr<~")" ^^ {
      case e => e
    }
   
  def event: Parser[Event] =   basicevent | brexp
  
   
  def apply(input: String): Option[EventList] = {
     
    parseAll(expr, input) match {
      case Success(result, _) => Some(new EventList(result.getEventList))
      case failure : NoSuccess => None
    }
  }
   
}



object EventListParser extends EventListParser with  ClassicEventParser{

  def basicevent = classicevent
   
   

}