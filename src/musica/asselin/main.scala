package musica.asselin

import musica.math._
import musica.symbol._
import scala.swing._
import scala.swing.BorderPanel.Position._
import scala.swing.GridBagPanel._
import event._
import musica.symbol.ClassicNote.fromString
import scala.swing.ComboBox.stringEditor
import javax.swing.SwingConstants
import java.io._

case class Preset(name: String, start: String, comma: String, steps: String)

object main extends SimpleSwingApplication {
  
  val presets_meantone: List[Preset] = List(
      Preset("Pythagorean","Gb","S","0,0,0,0,0,0,0,0,0,0,0"),
      Preset("1/3 meantone","Eb","S","-1/3,-1/3,-1/3,-1/3,-1/3,-1/3,-1/3,-1/3,-1/3,-1/3,-1/3"), 
      Preset("2/7 meantone","Eb","S","-2/7,-2/7,-2/7,-2/7,-2/7,-2/7,-2/7,-2/7,-2/7,-2/7,-2/7"),
      Preset("1/4 meantone","Eb","S","-1/4,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4"),
      Preset("1/5 meantone","Eb","S","-1/5,-1/5,-1/5,-1/5,-1/5,-1/5,-1/5,-1/5,-1/5,-1/5,-1/5"),
      Preset("1/6 meantone","Eb","S","-1/6,-1/6,-1/6,-1/6,-1/6,-1/6,-1/6,-1/6,-1/6,-1/6,-1/6"),
      Preset("1/8 meantone","Eb","S","-1/8,-1/8,-1/8,-1/8,-1/8,-1/8,-1/8,-1/8,-1/8,-1/8,-1/8"),
      Preset("1/10 meantone","Eb","S","-1/10,-1/10,-1/10,-1/10,-1/10,-1/10,-1/10,-1/10,-1/10,-1/10,-1/10"),
      Preset("equal","Ab","S","-1/11,-1/11,-1/11,-1/11,-1/11,-1/11,-1/11,-1/11,-1/11,-1/11,-1/11")
      )
    val presets_germanitalian: List[Preset] = List(    
      Preset("Kirnberger II","Db","S","0,0,0,0,0,0,0,-1/2,-1/2,0,0"),
      Preset("Kirnberger III","Db","S","0,0,0,0,0,-1/4,-1/4,-1/4,-1/4,0,0"),
      Preset("Werckmeister III","Eb","P","0,0,0,-1/4,-1/4,-1/4,0,0,-1/4,0,0"),
      Preset("Werckmeister IV","Eb","P","+1/3,-1/3,0,-1/3,0,-1/3,0,-1/3,0,-1/3,0"),
      Preset("Valotti","Eb","P","0,0,0,-1/6,-1/6,-1/6,-1/6,-1/6,-1/6,0,0"),
      Preset("Barca","Bb","S","0,0,-1/6,-1/6,-1/6,-1/6,-1/6,-1/6,0,0,0"),
      Preset("Lehman-Bach","Eb","P","-1/12,+1/12,-1/6,-1/6,-1/6,-1/6,-1/6,0,0,0,-1/12")
      )
     val presets_french: List[Preset] = List( 
      Preset("Chaumont","Eb","S","-1/6,-1/6,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4,-1/4")
      )
  
  
  def top = new MainFrame {
    
    
     title = "Asselin" 
       
     
     val selectcomma = new ComboBox(List("Syntonic","Pythagorean"))
     val selectstart = new ComboBox(List("Cb","Gb","Db","Ab","Eb","Bb","F","C"))
     val computebutton = new Button { text = "Compute" }
     
     class StepPanel(val nr: Int) {
      val label = new Label(""+nr+":") { horizontalAlignment = Alignment.Center }
      val valuefield = new TextField {  columns = 10 } 
      }

     val steps = Array.range(1 ,12).map(a => new StepPanel(a))
     
     val scalepanel = new GridBagPanel { grid =>
          val cl = new Constraints
          cl.fill = Fill.Horizontal
          cl.insets = new Insets(1,5,1,5)
          cl.anchor = Anchor.West
          cl.weightx = 1.0
          cl.weighty = 1.0  
          
          cl.grid = (1,1) ;   layout( new Label("Comma:") { horizontalAlignment = Alignment.Right }) = cl
          cl.grid = (2,1) ;   layout(selectcomma) = cl       
          cl.grid = (1,2) ;   layout( new Label("Starting fifth:") { horizontalAlignment = Alignment.Left }) = cl
          cl.grid = (2,2) ;   layout(selectstart) = cl       
 
           steps.foreach(a =>  {
              cl.grid = (1,a.nr+3) ;   layout( a.label) = cl
              cl.grid = (2,a.nr+3) ;   layout( a.valuefield) = cl
           })
           
            cl.grid = (2,15)
            layout(computebutton) = cl
     }
     
     val headers = Array("Note","Fifths", "Maj.Thirds", "Min.Thirds", "Cents")
     val data = Array.tabulate[Any](12, 5) {(a,b) => ""}
     val table = new Table(data, headers)  
     val scroll = new ScrollPane {
       border = Swing.EmptyBorder(1)
        contents = table
     }

    
     val namefield = new TextField { columns=20 }
     val namepanel = new FlowPanel {
       
       contents += new Label("Name:") 
       contents += namefield
     }
     
     val explanation = new TextArea {
       border = Swing.EmptyBorder(5)
       editable = false
       text = "Temperament definition using the Circle of Fifths (after Asselin).\n" +
              "Select the comma, starting fifth and specify per fifth the deviation from pure.\n" +
              "Use + or -  and a fraction of the comma (or 0 for a pure fifth).\n" +
              "The left panel shows the resulting deviations from pure intervals in cents." 
       
     }
     
     val leftarea = new BorderPanel {
       border = Swing.EmptyBorder(5)
      layout(namepanel) = North
      layout(scroll) = Center
      layout(explanation) = South
   
    }
     
     
    contents = new BorderPanel {
      layout(scalepanel) = West
      layout(leftarea) = Center
   
    }
   


      def clip(d: Double) = (d*10).toInt/10.0
      
    
      
      def getFifthTuning() = {
           val input = steps.map(a => a.valuefield.text).mkString(",")
           val comma = selectcomma.item.substring(0,1)
           FifthTuning(""+selectstart.item + ","+comma+","+input, namefield.text)
      }
      
      def getTuning() = {
          getFifthTuning.mappedTuning
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
      
      //compute
      
      
      
      def changelabels() = {
         val start: ClassicNote = selectstart.item
         val sf = start.fifth
         for (i <- 0 to 10) {
           steps(i).label.text = ClassicNote.FifthCircle(i + sf).toString + " - " + ClassicNote.FifthCircle(i + sf+1).toString +  ":"
         }
        
      }
      
        
      def loadpreset(preset: Preset) {
        namefield.text = preset.name
        preset.steps.split(",").zip(steps).foreach(e => {e._2.valuefield.text = e._1})
        selectstart.selection.item = preset.start
        selectcomma.selection.item = if (preset.comma== "S") "Syntonic" else "Pythagorean"
      }
      
      def useTuning(u: FifthTuning) {
         namefield.text = u.name
         u.devs.zipWithIndex.foreach(c => c match {case (a,i) => { 
            steps(i).valuefield.text = if (a.numer==0) "0" else { if (a.value>0) "+"+a.toString else
              a.toString} 
         }})
         selectstart.selection.item = (ClassicNote.FifthCircle(u.start)).toString
         selectcomma.selection.item = if (u.comma== PureInterval.SyntonicComma) "Syntonic" else "Pythagorean"
      }
      
      def load() {
         val chooser = new FileChooser(new File("."))
         //chooser.fileSelectionMode = FileChooser.SelectionMode.DirectoriesOnly
         chooser.title = "Open file"      
         val result = chooser.showOpenDialog(null) 
                if (result == FileChooser.Result.Approve) {
                var file = chooser.selectedFile
                FifthTuning.loadXML(file.getAbsolutePath()) match {
                  case Right(s) => Dialog.showMessage(null, s, "", Dialog.Message.Error)
                  case Left(u) => useTuning(u)
                }
            }      
      }
      
      def save() {
         val chooser = new FileChooser(new File("."))
         chooser.fileSelectionMode = FileChooser.SelectionMode.DirectoriesOnly
         chooser.title = "Save to dir"
             val result = chooser.showSaveDialog(null)
             if (result == FileChooser.Result.Approve) {
                var dir = chooser.selectedFile.getAbsolutePath()
                getFifthTuning.save(dir, namefield.text, namefield.text)
                Dialog.showMessage(null, "Exported to file", "")
            }
      }
      
      
      def exportScala() {
         val chooser = new FileChooser(new File("."))
         chooser.fileSelectionMode = FileChooser.SelectionMode.DirectoriesOnly
         chooser.title = "Save to dir"
             val result = chooser.showSaveDialog(null)
             if (result == FileChooser.Result.Approve) {
                var dir = chooser.selectedFile.getAbsolutePath()
                getTuning.exportScl(dir, namefield.text, namefield.text)
                Dialog.showMessage(null, "Exported to .scl file", "")
            }        
      }
      
      def exportHauptwerk () {
        val d = new Dialog
        val nametext = new TextField(columns = 30)
        val shortnametext = new TextField(columns = 30)
        val filetagtext = new TextField(columns = 30)
        val idtext = new TextField(columns = 10)
        val versiontext = new TextField{ columns = 10; text="1.0" }
        val cancelbutton = new Button("Cancel")
        val savebutton = new Button("Save")
        
        nametext.text = namefield.text
        
        //d.modal = true
        d.open
        d.peer.setLocationRelativeTo(null)
        d.title = "Export to Hauptwerk"
        d.contents = new GridBagPanel { grid =>
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
            
             if (nametext.text == "" ||  shortnametext.text == "" ||
                 filetagtext.text == "" ||  idtext.text == "" || versiontext.text == "") {
               
               Dialog.showMessage(null, "Pleae fill all fields", "", Dialog.Message.Error)
               
             }  else {          
               
             val chooser = new FileChooser(new File("."))
             chooser.fileSelectionMode = FileChooser.SelectionMode.DirectoriesOnly
             chooser.title = "Save to dir"
             val result = chooser.showSaveDialog(null)
             if (result == FileChooser.Result.Approve) {
                var dir = chooser.selectedFile.getAbsolutePath()
                getTuning.exportHauptwerk(dir, nametext.text, shortnametext.text,
                 filetagtext.text, idtext.text, versiontext.text)
                d.close()
                Dialog.showMessage(null, "Exported to Hauptwerk Temperament file", "")
            }}}
         }
        
      }
      
      menuBar = new MenuBar {
            
      contents += new Menu("Presets") {
        
          contents += new Menu("Meantone") {
           presets_meantone.foreach(p => {contents += new MenuItem(Action(p.name) {loadpreset(p)})})
          }
          
           contents += new Menu("German/Italian") {
           presets_germanitalian.foreach(p => {contents += new MenuItem(Action(p.name) {loadpreset(p)})})
          }
           
           contents += new Menu("French") {
           presets_french.foreach(p => {contents += new MenuItem(Action(p.name) {loadpreset(p)})})
          }           
           
                
          
      }
      
      contents += new Menu("File") {
           contents += new MenuItem(Action("Load") {
                 load()
             })
          contents += new MenuItem(Action("Save") {
                 save()
             })
          contents += new MenuItem(Action("Export to Hauptwerk") {
                 exportHauptwerk()
             })
          contents += new MenuItem(Action("Export to Scala .scl") {
                 exportScala()
             })
      }
      
      }
         
      
   listenTo(computebutton, selectstart.selection, selectcomma.selection) 
   
  reactions += {
    case WindowClosing(e) => System.exit(0)
    case ButtonClicked(`computebutton`) => { compute() }
    case SelectionChanged(`selectstart`) => { 
      changelabels() 
      compute() }
    case SelectionChanged(`selectcomma`) => { compute() }
  }
    
    loadpreset(presets_meantone(0))
    this.peer.setSize(700,500)
    this.peer.setLocationRelativeTo(null)
    
  }
  
}  