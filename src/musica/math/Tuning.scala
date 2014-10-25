package musica.math
import musica.symbol._
import scala.xml.XML
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
    
 // def mapTo(scale: ClassicScale, base: ClassicNote)  = new MappedTuning(steplist, scale, base)
 
 def save(path: String, name: String, filetag: String, 
               version: String =  "1.0"): Unit = {
     
     XML.save(path+"/"+filetag+".Tuning_Musica_xml",
         
 <musica FileFormat="Tuning" FileFormatVersion="1.00">
  <head>
	<name>{ name }</name>
	<version>{ version }</version>
    <size>{ size }</size>
  </head>
  <steplist>
   { steplist.zipWithIndex.map ( s => 
     <step>
       <index>{ s._2 }</index>
       <value>{ s._1.toString }</value>
     </step> )}
  </steplist>
 </musica>,
                   
   "UTF-8", true, null)   
   }
   
   def exportScl(path: String, filetag: String, name: String) {
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
  def apply(st: String) = new Tuning(st.split(",").toList.map(s => RealInterval(s)))  
  
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

  def loadXML(filename: String): Tuning = {
    
    val tuningElem = scala.xml.XML.loadFile(filename)
    
    val stepstoomany = ( tuningElem \ "steplist" ).map { steplist => { 
     (steplist \ "step").map (step => (step \ "value").text)
    }}
    val steps = stepstoomany(0).map(s => RealInterval(s)).toList
    new Tuning(steps)
  }
}




