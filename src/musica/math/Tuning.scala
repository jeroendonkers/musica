package musica.math
import musica.symbol._
import scala.xml.XML
import musica.io._
import java.io._

class Tuning(val steplist: List[RealInterval], val name: String = "") {
   def step(i: Int): RealInterval = {
     if (size == 0) {
       RealInterval(0)
     } else {
       val octave = if (i>=0) (i / size) else  ((i+1) / size) - 1  
       val idx = if (i>=0) (i % size) else (size-1- ((-i-1) % size))    
       (PureInterval(2,1) * octave) + steplist(idx)
     }
   }
   val size = steplist.size   
   
   val centlist = steplist.map(i => i.cents)
   val valuelist = steplist.map(i => i.value)
   
   def -(that: Tuning) = Tuning(steplist.zip(that.steplist).map(e => 
      e match {case (a,b) => a-b }))
   def +(that: Tuning) = Tuning(steplist.zip(that.steplist).map(e => 
      e match {case (a,b) => a+b }))
   
      
  def intervals(stepsize: Int) = Tuning(steplist.zipWithIndex.map(e =>
    e match {case (a,i) => step(i+stepsize)-a }
    ))
    
  
 
 def saveXML(path: String, filetag: String, name: String, version: String = "1.0"): Unit = {
     Tuning.xmlfile.saveXML(path, filetag, new MusicaXmlData(
        Map("name" -> name, "version" -> version, "size"-> size.toString),
        Map("value" -> steplist.map(s => s.toString))   
     ))
   }   
  
   
   def exportScl(path: String, filetag: String, name: String, version: String) {
      val w = new BufferedWriter(new FileWriter(path+"/"+filetag+".scl"))
      w.write("! "+filetag+".scl\n")
      w.write("!\n")
      w.write(name+"\n")
      w.write(size+"\n")
      w.write("!\n")
      List.range(1,size+1).foreach(i => {
        step(i) match {
          case c: CentsInterval => w.write(c.cents+"\n")
          case r: RealInterval => w.write(r.toString+"\n")
        }
      })
      w.close
   }
}


object Tuning {
  
  def apply(i: RealInterval, n: Int) = new Tuning(List.range(1,n+1).map(e => i))
  def apply(step: List[RealInterval]) = new Tuning(step)
  def apply(steps: RealInterval*) = new Tuning(steps.toList)
  def apply(st: String) = new Tuning(st.split(",").toList.map(RealInterval(_)))  
  
  def ET(n: Int) = Tuning(List.range(1,n+1).map(i => CentsInterval((1200.0 * (i-1)) / n))) 

  val ET12 = ET(12)
  
  def fromRatios(rs: List[PureInterval]): Tuning = {
    def incsum(i: PureInterval, a: List[PureInterval]): List[PureInterval] = {
      a match {
        case List() => List()
        case x :: b => i+x :: incsum(i+x, b)
      }
    }
    new Tuning(PureInterval(1,1) :: incsum(PureInterval(1,1),rs))
  }
  def fromRatios(iis: PureInterval*): Tuning = fromRatios(iis.toList)
  def fromRatios(st: String): Tuning = { 
     fromRatios(st.split(",").toList.map(s => PureInterval(s)))
  }

  val xmlfile = new MusicaXmlFile("Tuning", "1.0", "steplist", "step") 
  
  def loadXML(filename: String): Either[Tuning, String] = {
    
     xmlfile.loadXML(filename, new MusicaXmlData(
        Map("name" ->"" , "version" -> "", "size"-> ""),
        Map("value" -> List()))) match {
        case Left(m) => {
           val name = m.header("name")
           val size = m.header("size").toInt
           val steps = m.data("value")
           if (steps.size != size) Right("Error in file: number of steps not equal to size.")
           Left(new Tuning(steps.map(RealInterval(_)),name))
       }
       case Right(s) => Right(s)
     }
 
  }
}




