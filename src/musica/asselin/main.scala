package musica.asselin

import musica.math._
import musica.symbol._
import scala.swing._
import scala.swing.BorderPanel.Position._
import event._
import musica.symbol.ClassicNote.fromString
import scala.swing.ComboBox.stringEditor

class StepPanel(val nr: Int) extends FlowPanel {
  val label = new Label(""+nr+":")
  val valuefield = new TextField {
    columns = 10
    text = "-1/4"
  }
  contents += label
  contents += valuefield
}


case class Preset(name: String, start: String, comma: String, steps: String)

object main extends SimpleSwingApplication {
  
  val presets: List[Preset] = List(
      Preset("Pythagorean","Gb","S","0,0,0,0,0,0,0,0,0,0,0"),
      Preset("1/3 meantone","Eb","S","-1/3,-1/3,-1/3,-1/3,-1/3,-1/3,-1/3,-1/3,-1/3,-1/3,-1/3"),
      Preset("1/4 meantone","Eb","S","-1/4,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4"),
      Preset("1/5 meantone","Eb","S","-1/5,-1/5,-1/5,-1/5,-1/5,-1/5,-1/5,-1/5,-1/5,-1/5,-1/5"),
      Preset("1/6 meantone","Eb","S","-1/6,-1/6,-1/6,-1/6,-1/6,-1/6,-1/6,-1/6,-1/6,-1/6,-1/6"),
      Preset("2/7 meantone","Eb","S","-2/7,-2/7,-2/7,-2/7,-2/7,-2/7,-2/7,-2/7,-2/7,-2/7,-2/7"),
      Preset("equal","Ab","S","-1/11,-1/11,-1/11,-1/11,-1/11,-1/11,-1/11,-1/11,-1/11,-1/11,-1/11"),
      Preset("Kirnberger II","Db","S","0,0,0,0,0,0,0,-1/2,-1/2,0,0"),
      Preset("Kirnberger III","Db","S","0,0,0,0,0,-1/4,-1/4,-1/4,-1/4,0,0"),
      Preset("Werckmeister III","Eb","P","0,0,0,-1/4,-1/4,-1/4,0,0,-1/4,0,0"),
      Preset("Werckmeister IV","Eb","P","+1/3,-1/3,0,-1/3,0,-1/3,0,-1/3,0,-1/3,0"),
      Preset("Valotti","Eb","P","0,0,0,-1/6,-1/6,-1/6,-1/6,-1/6,-1/6,0,0"),
      Preset("Barca","Bb","S","0,0,-1/6,-1/6,-1/6,-1/6,-1/6,-1/6,0,0,0"),
      Preset("Chaumont","Eb","S","-1/6,-1/6,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4")
      )
  
  
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
     
     val headers = Array("Note","Fifths", "Maj.Thirds", "Min.Thirds", "Cents")
     val data = Array.tabulate[Any](12, 5) {(a,b) => ""}
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
       val comma = selectcomma.item.substring(0,1)
       val tuning = Tuning.fromFifths(""+selectstart.item + ","+comma+","+input)
       val fc = tuning.compare(ClassicInterval.Fifth,PureInterval.Fifth).centlist
       val tc = tuning.compare(ClassicInterval.MajorThird,PureInterval.MajorThird).centlist
       val mc = tuning.compare(ClassicInterval.MinorThird,PureInterval.MinorThird).centlist
       for (i <- 0 to 11) {
          val step = tuning.mappedStep(i)
          data(i)(0) = step._1
          data(i)(4) = clip(step._2.cents)
          data(i)(1) = clip(fc(i))
          data(i)(2) = clip(tc(i))
          data(i)(3) = clip(mc(i))
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
      
      def load(start: String, comma: String, stepinput: String) {
        stepinput.split(",").zip(steps).foreach(e => {e._2.valuefield.text = e._1})
        selectstart.selection.item = start
        selectcomma.selection.item = if (comma== "S") "Syntonic" else "Pythagorean"
      }
      
      
      menuBar = new MenuBar {
            
      contents += new Menu("Presets") {
          presets.foreach(p => {
             contents += new MenuItem(Action(p.name) {
                title = "Asselin: "+p.name
                load(p.start, p.comma, p.steps)
             })
            
          }
            
          )
      }
      
      }
      
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