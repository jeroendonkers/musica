import musica.math._
import musica.symbol._
import scala.swing._
import scala.swing.BorderPanel.Position._
import java.awt.Toolkit
import event._
import collection.mutable.ArrayBuffer

class StepPanel(val nr: Int) extends FlowPanel {
  val label = new Label(""+nr+":")
  val valuefield = new TextField {
    columns = 10
    text = "-1/4"
  }
  contents += label
  contents += valuefield
}

object main extends SimpleSwingApplication {
  
  def top = new MainFrame {
    
    
     title = "Asselin" 
     val scalepanel = new GridPanel(14,1)
     
      val commabox = new FlowPanel
     commabox.contents += new Label("Comma:")
     val selectcomma = new ComboBox(List("Syntonic","Pythagorean"))
     commabox.contents += selectcomma
     scalepanel.contents += commabox
     
     val startbox = new FlowPanel
     startbox.contents += new Label("Starting fifth:")
     val selectstart = new ComboBox(List("Cb","Gb","Db","Ab","Eb","Bb","F","C"))
     startbox.contents += selectstart
     scalepanel.contents += startbox
     
     val steps = Array.range(1 ,12).map(a => new StepPanel(a))
     steps.foreach(a => scalepanel.contents+= a)
      
     val button = new Button {
        text = "Compute"
      }
     scalepanel.contents += button
     
     val headers = Array("Note","Fifths", "Thirds", "Cents")
     val data = Array.tabulate[Any](12, 4) {(a,b) => ""}
     val table = new Table(data, headers)
  
     val scroll = new ScrollPane {
        contents = table
     }

    
    contents = new BorderPanel {
      layout(scalepanel) = West
      layout(scroll) = Center
      
    }
   
      listenTo(button, selectstart.selection, selectcomma.selection) 

      def clip(d: Double) = (d*10).toInt/10.0
      
      def compute() = {
       val input = steps.map(a => a.valuefield.text).mkString(",")
       val comma = if (selectcomma.item == "Syntonic") PureInterval.SyntonicComma else PureInterval.PythagoreanComma 
       val tuning = Tuning.fromFifths(""+selectstart.item + ","+input, comma)
       val fc = tuning.compare(ClassicInterval.Fifth,PureInterval.Fifth).centlist
       val tc = tuning.compare(ClassicInterval.MajorThird,PureInterval.MajorThird).centlist
       for (i <- 0 to 11) {
          val step = tuning.mappedStep(i)
          data(i)(0) = step._1
          data(i)(3) = clip(step._2.cents)
          data(i)(1) = clip(fc(i))
          data(i)(2) = clip(tc(i))
          table.repaint
       }
      }
      
      compute
      
      def changelabels() = {
         val start: ClassicNote = selectstart.item
         val sf = start.fifth
         for (i <- 0 to 10) {
           steps(i).label.text = ClassicNote.FifthCircle(i + sf).toString + "-" + ClassicNote.FifthCircle(i + sf+1).toString +  ":"
         }
        
      }
      
      changelabels
      
      
  reactions += {
    case WindowClosing(e) => System.exit(0)
    case ButtonClicked(`button`) => { compute() }
    case SelectionChanged(`selectstart`) => { 
      changelabels() 
      compute() }
    case SelectionChanged(`selectcomma`) => { compute() }
  }
    
    
    this.peer.setSize(500,500)
    this.peer.setLocationRelativeTo(null)
    
  }
  
}  