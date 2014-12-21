package musica.symbol
import scala.util.parsing.combinator._
import musica.classic.ClassicEventParser
import musica.math._
import scala.collection.mutable.Map 


trait BasicParser extends RegexParsers {
  
   def slash: Parser[String] = "/"
   def hat: Parser[String] = "^"   
    
   def int: Parser[Int] = """\d+""".r ^^ {
     case a => a.toInt
   } 
  
   def valuecode: Parser[SymbolicTime] = """:[WHQ8624]""".r ^^ {
     case ":W" => 1\1
     case ":H" => 1\2
     case ":Q" => 1\4
     case ":8" => 1\8
     case ":6" => 1\16
     case ":2" => 1\32
     case ":4" => 1\64
   } 
   
    def valuecodedot: Parser[SymbolicTime] = valuecode ~ "." ^^ {
     case t ~ "." => t * 3\2
   }
   def valuecodedotdot: Parser[SymbolicTime] = valuecode ~ ".." ^^ {
      case t ~ ".." => t * 7\4
   }
   
   def timeexpr: Parser[SymbolicTime] = "[" ~> int ~ slash ~ int <~ "]" ^^ {
      case n ~ s ~ m =>  SymbolicTime(n,m)
   }
   
   def valueexpr: Parser[SymbolicTime] = "v" ~ timeexpr ^^ {
     case "v" ~ t => t
   }
   
   def durationexpr: Parser[SymbolicTime] = "d" ~ timeexpr ^^ {
     case "d" ~ t => t
   }
   
   def value: Parser[SymbolicTime] = valuecodedotdot | valuecodedot | valuecode | valueexpr
      
   def durationcode: Parser[SymbolicTime] = 
     "_" ^^^ SymbolicTime(1\1) | 
     "-" ^^^ SymbolicTime(3\4) |
     "*" ^^^ SymbolicTime(1\2) |
     "'" ^^^ SymbolicTime(1\4)
   
     
  def duration: Parser[SymbolicTime] =  durationcode | durationexpr    
     
   def rest = "R"
     
   def restevent: Parser[Rest] = rest ~ value ^^ {
     case r ~ t =>  new Rest(t) 
   } | rest ^^ {
     case r => new Rest(0/1)
   }
   
}


trait EventListParser extends JavaTokenParsers  with BasicParser {
  
  
   def selectBasicEvent(s: String): Parser[Event] // too
     
   private var basicevent = selectBasicEvent("")
   
   private val eventmap:Map[String,Event] = Map()  
   
   def define(s: String, e: Event) = {
    if (isdefined(s)) eventmap(s.toLowerCase) = e 
    else eventmap += (s.toLowerCase -> e)
  }
   
  def isdefined(s: String) = {
   eventmap.isDefinedAt(s.toLowerCase)
  }
  
  def getevent(s: String): Event = {
    if (eventmap.isDefinedAt(s.toLowerCase)) eventmap(s.toLowerCase) else null
  }
  
  def clearmap() = { 
    eventmap.clear()
  }
   
  def idfactor : Parser[Event] = ident ^^ { 
      case id => 
        if (isdefined(id)) getevent(id) 
        else {
          null
        }  
    }
  


  
  
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
   
  def event: Parser[Event] =   basicevent | restevent | brexp | idfactor
  
  
  def command: Parser[String] = ">"~ident ^^{
    case ">"~id => {
      if (id == "eitz") { basicevent = selectBasicEvent("eitz"); ""}
      else ""
    }
  }
  
  def statement: Parser[String] = ident~"="~event ^^ {
      case id ~ "=" ~expr => { if (!isdefined(id)) { define(id,expr)}; id }
  }  | command
  
  def script: Parser[List[String]] = repsep(statement,";") <~ ";" | repsep(statement,";")
  
  def apply(input: String, bselect: String = ""): Option[EventList] = {
     
    val newinput =  input.split("\n").map(_.split("//")(0)).mkString("\n")
    clearmap()
    
    basicevent = selectBasicEvent(bselect)
    
    parseAll(script, newinput) match {
      case Success(result, _) => {
        val result = getevent("play")
        if (result == null) None else
        Some(new EventList(result.getEventList))
      }
      case failure : NoSuccess => None
    }
  }
   
}



object EventListParser extends EventListParser with  ClassicEventParser{

  def selectBasicEvent(s: String): Parser[Event] = {
    s match {
      case "eitz" => eitzevent
      case _ => classicnoteevent 
    }
  }

}




