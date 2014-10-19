package musica.asselin

import musica.math._
import musica.symbol._
import scala.swing._
import scala.swing.BorderPanel.Position._
import event._
import musica.symbol.ClassicNote.fromString
import scala.swing.ComboBox.stringEditor
import javax.swing.SwingConstants
import java.io._

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
  
  val presets_meantone: List[Preset] = List(
      Preset("Pythagorean","Gb","S","0,0,0,0,0,0,0,0,0,0,0"),
      Preset("1/3 meantone","Eb","S","-1/3,-1/3,-1/3,-1/3,-1/3,-1/3,-1/3,-1/3,-1/3,-1/3,-1/3"),
      Preset("1/4 meantone","Eb","S","-1/4,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4"),
      Preset("1/5 meantone","Eb","S","-1/5,-1/5,-1/5,-1/5,-1/5,-1/5,-1/5,-1/5,-1/5,-1/5,-1/5"),
      Preset("1/6 meantone","Eb","S","-1/6,-1/6,-1/6,-1/6,-1/6,-1/6,-1/6,-1/6,-1/6,-1/6,-1/6"),
      Preset("2/7 meantone","Eb","S","-2/7,-2/7,-2/7,-2/7,-2/7,-2/7,-2/7,-2/7,-2/7,-2/7,-2/7"),
      Preset("equal","Ab","S","-1/11,-1/11,-1/11,-1/11,-1/11,-1/11,-1/11,-1/11,-1/11,-1/11,-1/11")
      )
    val presets_germanitalian: List[Preset] = List(    
      Preset("Kirnberger II","Db","S","0,0,0,0,0,0,0,-1/2,-1/2,0,0"),
      Preset("Kirnberger III","Db","S","0,0,0,0,0,-1/4,-1/4,-1/4,-1/4,0,0"),
      Preset("Werckmeister III","Eb","P","0,0,0,-1/4,-1/4,-1/4,0,0,-1/4,0,0"),
      Preset("Werckmeister IV","Eb","P","+1/3,-1/3,0,-1/3,0,-1/3,0,-1/3,0,-1/3,0"),
      Preset("Valotti","Eb","P","0,0,0,-1/6,-1/6,-1/6,-1/6,-1/6,-1/6,0,0"),
      Preset("Barca","Bb","S","0,0,-1/6,-1/6,-1/6,-1/6,-1/6,-1/6,0,0,0")
      )
     val presets_french: List[Preset] = List( 
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
      
      def getTuning() = {
           val input = steps.map(a => a.valuefield.text).mkString(",")
           val comma = selectcomma.item.substring(0,1)
           Tuning.fromFifths(""+selectstart.item + ","+comma+","+input)
      }
      
      def compute() = {
       val tuning = getTuning
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
      
      def load(preset: Preset) {
        title = "Asselin: "+preset.name
        preset.steps.split(",").zip(steps).foreach(e => {e._2.valuefield.text = e._1})
        selectstart.selection.item = preset.start
        selectcomma.selection.item = if (preset.comma== "S") "Syntonic" else "Pythagorean"
      }
      
      load(presets_meantone(0))
      
      def exportHauptwerk () {
        val d = new Dialog
        val nametext = new TextField(columns = 30)
        val shortnametext = new TextField(columns = 30)
        val filetagtext = new TextField(columns = 30)
        val idtext = new TextField(columns = 10)
        val versiontext = new TextField{ columns = 10; text="1.0" }
        val cancelbutton = new Button("Cancel")
        val savebutton = new Button("Save")
        
        //d.modal = true
        d.open
        d.peer.setLocationRelativeTo(null)
        d.title = "Export to Hauptwerk"
        d.contents = new GridBagPanel { grid =>
          import GridBagPanel._
          val cl = new Constraints
           cl.fill = Fill.Horizontal
           cl.insets = new Insets(5,5,5,5)
           cl.anchor = Anchor.West
           cl.weightx = 1.0
           cl.weighty = 1.0
                
           cl.grid = (1,1) ;   layout( new Label("Name:") { horizontalAlignment = Alignment.Left }) = cl
           cl.grid = (2,1) ;   layout(nametext) = cl 
           cl.grid = (1,2) ;   layout(new Label("Short name:") { horizontalAlignment = Alignment.Left }) = cl
           cl.grid = (2,2) ;   layout(shortnametext) = cl
           cl.grid = (1,3) ;   layout(new Label("File name tag:") { horizontalAlignment = Alignment.Left }) = cl
           cl.grid = (2,3) ;   layout(filetagtext) = cl 
           cl.grid = (1,4) ;   layout(new Label("Unique ID:") { horizontalAlignment = Alignment.Left }) = cl
           cl.grid = (2,4) ;   layout(idtext) = cl
           cl.grid = (1,5) ;   layout(new Label("Version:") { horizontalAlignment = Alignment.Left }) = cl
           cl.grid = (2,5) ;   layout(versiontext) = cl 
           
           val buttons = new FlowPanel {
              contents += cancelbutton
              contents += savebutton
           }
           cl.grid = (2,6) ;   layout(buttons) = cl         
           border = Swing.EmptyBorder(5, 5, 5, 5)
        }
        d.listenTo(savebutton, cancelbutton)
        d.reactions += {
           case ButtonClicked(`cancelbutton`) => { d.close() }
           case ButtonClicked(`savebutton`) => {
             val specs = new HauptwerkSpecs(nametext.text, shortnametext.text,
                 filetagtext.text, idtext.text, versiontext.text)
            
             if (specs.name =="" || specs.shortname=="" ||  specs.filetag=="" ||
                 specs.id == "" || specs.version=="") {
               
               Dialog.showMessage(null, "Pleae fill all fields", "", Dialog.Message.Error)
               
             }  else {          
               
             val chooser = new FileChooser(new File("."))
             chooser.fileSelectionMode = FileChooser.SelectionMode.DirectoriesOnly
             chooser.title = "Save to dir"
             val result = chooser.showSaveDialog(null)
             if (result == FileChooser.Result.Approve) {
                var dir = chooser.selectedFile.getAbsolutePath()
                getTuning.exportHauptwerk(dir, specs)
                d.close()
                Dialog.showMessage(null, "Exported to Hauptwerk Temperament file", "")
            }}}
         }
        
      }
      
      menuBar = new MenuBar {
            
      contents += new Menu("Presets") {
        
          contents += new Menu("Meantone") {
           presets_meantone.foreach(p => {contents += new MenuItem(Action(p.name) {load(p)})})
          }
          
           contents += new Menu("German/Italian") {
           presets_germanitalian.foreach(p => {contents += new MenuItem(Action(p.name) {load(p)})})
          }
           
           contents += new Menu("French") {
           presets_french.foreach(p => {contents += new MenuItem(Action(p.name) {load(p)})})
          }           
           
                
          
      }
      
      contents += new Menu("File") {
          contents += new MenuItem(Action("Export to Hauptwerk") {
                 exportHauptwerk()
             })
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