package musica.asselin

import musica.math._
import musica.symbol._
import musica.classic._
import musica.io._
import scala.swing._
import scala.swing.BorderPanel.Position._
import scala.swing.GridBagPanel._
import event._

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
     val fillbutton = new Button { text = "Fill" }
     
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
            cl.grid = (1,15)
            layout(fillbutton) = cl
            cl.grid = (2,15)
            layout(computebutton) = cl
     }
     
     val headers = Array("Note","Fifths", "Maj.Thirds", "Min.Thirds", "H.Sevenths", "Cents")
     val data = Array.tabulate[Any](14, 6) {(a,b) => ""}
     val table = new Table(data, headers)  
     val scroll = new ScrollPane {
       border = Swing.EmptyBorder(1)
        contents = table
     }

    
     val namefield = new TextField { columns=20 }
     val namepanel = new FlowPanel(scala.swing.FlowPanel.Alignment.Left)(    
       new Label("Name:"),namefield
     )
     
     val explanation = new TextArea {
       border = Swing.EmptyBorder(5)
       editable = false
       text = "Temperament definition using the Circle of Fifths.\n" +
              "Select the comma, starting fifth and specify per fifth the deviation from pure.\n" +
              "Use + or -  and a fraction of the comma (or 0 for a pure fifth).\n" +
              "The left panel shows the resulting deviations from pure intervals.\n" +
              "(Download at github.com/jeroendonkers/musica)"
       
     }
     
     val selectsort = new ComboBox(List("Scale","Fifths"))
     val selectvalues = new ComboBox(List("Cents","% Syntonic comma","% Pythagorean comma"))
     
     
     val optionpanel = new FlowPanel(scala.swing.FlowPanel.Alignment.Left)(      
         new Label("Sort on: "),  selectsort, new Label("Show deviations in: "), selectvalues 
     )
     
     val northpanel = new GridPanel(2,1) {
       contents += namepanel
       contents += optionpanel
       
     }
     
     val leftarea = new BorderPanel {
       border = Swing.EmptyBorder(5)
      layout(northpanel) = North
      layout(scroll) = Center
      layout(explanation) = South
   
    }
     
     
    contents = new BorderPanel {
      layout(scalepanel) = West
      layout(leftarea) = Center
   
    }
      def clip(d: Double) = ((d*10).round/10.0)
      
      def getFifthTuning() = {
           val input = steps.map(a => a.valuefield.text).mkString(",")
           val comma = selectcomma.item.substring(0,1)
           FifthTuning(""+selectstart.item + ","+comma+","+input, namefield.text)
      }
      
     
      def getfac(): Double = {
        selectvalues.selection.index match {
          case 0 =>  1
          case 1 =>  100/PureInterval.SyntonicComma.cents
          case 2 =>  100/PureInterval.PythagoreanComma.cents
        }
      }
      
      def presentDev(d: Double): Double = { clip(d * getfac)  }
      
      def orderIndex(c: ClassicNote, i: Int): Int = {
        selectsort.selection.index match {
          case 0 =>  i
          case 1 =>  11 - (selectstart.selection.index + 4 - c.fifth)
        }
      }
      
      def average(l: List[Double]): (Double, Double) = {
        var sum: Double = 0
        var sumsq: Double = 0
        val fac = getfac

        for (i <- 0 to 11) { sum += l(i)*fac; sumsq += l(i)*l(i)*fac*fac }
        (clip(sum/12), clip(Math.sqrt(sumsq/12 - (sum/12)*(sum/12))))
      }
      
      def compute() = {
       val tuning = getFifthTuning
       val fc = tuning.compare(Fifth,PureInterval.Fifth).centlist
       val tc = tuning.compare(MajorThird,PureInterval.MajorThird).centlist
       val mc = tuning.compare(MinorThird,PureInterval.MinorThird).centlist
       val sc = tuning.compare(MinorSeventh,PureInterval.HarmonicSeventh).centlist
       for (i <- 0 to 11) {
          val step = tuning.mappedStep(i)
          val j = orderIndex(step._1,i)
          data(j)(0) = step._1
          data(j)(5) = clip(step._2.cents)
          data(j)(1) = presentDev(fc(i))
          data(j)(2) = presentDev(tc(i))
          data(j)(3) = presentDev(mc(i))
          data(j)(4) = presentDev(sc(i))
       }
       val fav = average(fc)
       val tav = average(tc)
       val mav = average(mc)
       val sav = average(sc)
       
       data(12)(0) = "Average"
       data(13)(0) = "St.Dev"
       data(12)(5) = ""
       data(13)(5) = ""
         
       data(12)(1) = fav._1
       data(13)(1) = fav._2
       data(12)(2) = tav._1
       data(13)(2) = tav._2
       data(12)(3) = mav._1
       data(13)(3) = mav._2
       data(12)(4) = sav._1
       data(13)(4) = sav._2

       
       table.repaint
       
      }
      
      //compute
      
      
      
      def changelabels() = {
         val start: ClassicNote = selectstart.item
         val sf = start.fifth
         for (i <- 0 to 10) {
           steps(i).label.text = FifthCircle(i + sf).toString + " - " + FifthCircle(i + sf+1).toString +  ":"
         }
        
      }
      
      def fill() {
         for (i <- 1 to 10) {
           steps(i).valuefield.text = steps(0).valuefield.text
         }
         compute
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
         selectstart.selection.item = (FifthCircle(u.start)).toString
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
      
      
      var midifile: Option[String] = None
      
      def playMidiFile() {
        if (midifile.isDefined) {
          Midi.openMidiOut() 
          Midi.send(Midi.scaleTuning(getFifthTuning()),-1) 
           Midi.playMidiFile(midifile.get)
        }
      }
      
      def selectMidiFile() {
         val chooser = new FileChooser(new File("."))
         chooser.title = "Select midi file"      
         val result = chooser.showOpenDialog(null) 
                if (result == FileChooser.Result.Approve) {
                var file = chooser.selectedFile
                midifile = Some(file.getAbsolutePath())
                playMidiFile()
            }      
      }
        
   
      
      class SaveDialog(val savefunction: (String,String,String, String) => Unit, val ttl: String) extends Dialog() {
         val nametext = new TextField(columns = 30)
         val filetagtext = new TextField(columns = 30)  
         val cancelbutton = new Button("Cancel")
         val savebutton = new Button("Save")
         title = ttl
         contents = new GridBagPanel { grid =>
           val cl = new Constraints
           cl.fill = Fill.Horizontal
           cl.insets = new Insets(5,5,5,5)
           cl.anchor = Anchor.West
           cl.weightx = 1.0
           cl.weighty = 1.0
                
           cl.grid = (1,1) ;   layout( new Label("Description:") { horizontalAlignment = Alignment.Left }) = cl
           cl.grid = (2,1) ;   layout(nametext) = cl 
           cl.grid = (1,2) ;   layout(new Label("File name:") { horizontalAlignment = Alignment.Left }) = cl
           cl.grid = (2,2) ;   layout(filetagtext) = cl 
          
           val buttons = new FlowPanel {
              contents += cancelbutton
              contents += savebutton
           }
           cl.grid = (2,3) ;   layout(buttons) = cl         
           border = Swing.EmptyBorder(5, 5, 5, 5)
        }
        
        listenTo(savebutton, cancelbutton)
        reactions += {
           case ButtonClicked(`cancelbutton`) => { close() }
           case ButtonClicked(`savebutton`) => {
            
             if (nametext.text == "" ||  filetagtext.text == "" ) {
               
               Dialog.showMessage(null, "Pleae fill all fields", "", Dialog.Message.Error)
               
             }  else {          
               
             val chooser = new FileChooser(new File("."))
             chooser.fileSelectionMode = FileChooser.SelectionMode.DirectoriesOnly
             chooser.title = "Save to dir"
             val result = chooser.showSaveDialog(null)
             if (result == FileChooser.Result.Approve) {
                var dir = chooser.selectedFile.getAbsolutePath()
                savefunction(dir, filetagtext.text, nametext.text,"1.0")
                close()
                Dialog.showMessage(null, "File saved", "")
            }}}
         }
         
        override def open() {
           super.open
           nametext.text = namefield.text
           peer.setLocationRelativeTo(null)
           filetagtext.text == ""
             
         }
      }
      
      def save() {
        new SaveDialog(getFifthTuning.saveXML,"Save").open 
      }
      
      def exportScala() {
          new SaveDialog(getFifthTuning.exportScl,"Export to Scala").open
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
                
           cl.grid = (1,1) ;   layout( new Label("Description:") { horizontalAlignment = Alignment.Left }) = cl
           cl.grid = (2,1) ;   layout(nametext) = cl 
           cl.grid = (1,2) ;   layout(new Label("Short name:") { horizontalAlignment = Alignment.Left }) = cl
           cl.grid = (2,2) ;   layout(shortnametext) = cl
           cl.grid = (1,3) ;   layout(new Label("File name:") { horizontalAlignment = Alignment.Left }) = cl
           cl.grid = (2,3) ;   layout(filetagtext) = cl 
           cl.grid = (1,4) ;   layout(new Label("Unique ID: (80000-90000)") { horizontalAlignment = Alignment.Left }) = cl
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
                getFifthTuning.exportHauptwerk(dir, nametext.text, shortnametext.text,
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
      
      contents += new Menu("Play") {
           contents += new MenuItem(Action("Select and play Midi file") {
                selectMidiFile()
             })
             contents += new MenuItem(Action("Replay Midi file") {
                playMidiFile()
             })
             contents += new MenuItem(Action("Stop playing") {
                Midi.stopPlaying
             })     
           
      }
      
      }
         
      
   listenTo(computebutton, fillbutton, selectstart.selection, selectcomma.selection, 
       selectvalues.selection, selectsort.selection) 
   
  reactions += {
    case WindowClosing(e) => System.exit(0)
    case ButtonClicked(`computebutton`) => { compute() }
    case ButtonClicked(`fillbutton`) => { fill() }
    case SelectionChanged(`selectstart`) => { 
      changelabels() 
      compute() }
    case SelectionChanged(`selectcomma`) => { compute() }
    case SelectionChanged(`selectvalues`) => { compute() }
    case SelectionChanged(`selectsort`) => { compute() }
  }
    
    loadpreset(presets_meantone(0))
    this.peer.setSize(700,500)
    this.peer.setLocationRelativeTo(null)
    
  }
  
}  