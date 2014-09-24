import musica.math._

object main {

  def main(args: Array[String]): Unit = {
    
  val c = PureInterval(4,13)
  println(Math.log(c.value))
  println((Math.log(c.value)/Math.log(2)).floor.toInt)
  

}
  
}  