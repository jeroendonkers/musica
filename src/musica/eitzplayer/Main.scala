package musica.eitzplayer

import musica.math._
import musica.symbol._
import musica.classic._
import musica.io._
import scala.swing._
import scala.swing.BorderPanel.Position._
import scala.swing.GridBagPanel._
import event._
import java.awt.Font

import scala.swing.ComboBox.stringEditor
import javax.swing.SwingConstants
import java.io._
import scala.io.Source

object main extends SimpleSwingApplication {
  
  def top = new MainFrame {
  var loaded = false
  var changed = false
  var file: Option[File] = None
  var filename = "None"
  var instrument = "Bowed Glass"  
  var bpm = 80
    
  def makeTitle() = {
      title = "EitzPlayer: "+filename + (if (changed) "*"else "")  
   }
   makeTitle
   
   peer.setDefaultCloseOperation(javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE)  
   
    val ins = GeneralMidi.Instrument.toSeq.sortBy(_._2).map{ case (n,i) => n}   
    val instruments = new ComboBox(ins)
    instruments.selection.item = instrument
    
    val speedslider = new Slider() {
      min = 0 
      max = 220
      majorTickSpacing = 40
      value = bpm
      paintLabels=true
    }
    
    
    val textArea = new EditorPane  {
          preferredSize = new Dimension(500, 250)
          peer.setAutoscrolls(true)
          font = new Font("Courier", Font.BOLD,16)
    }
    val textpane = new ScrollPane 
    textpane.viewportView = textArea
   
     val playbutton = new Button {
      text = "Play"
      borderPainted = true
      enabled = true
      tooltip = "Click to play script"
      mnemonic = Key.P
    }
    val stopbutton = new Button {
      text = "Stop"
      borderPainted = true
      enabled = true
      tooltip = "Click to stop playing"
      mnemonic = Key.X
    }
      
    val buttonbar = new FlowPanel(scala.swing.FlowPanel.Alignment.Left)() {
      contents+=playbutton
      contents+=stopbutton     
      contents+=instruments
      contents += speedslider
    }

    contents = new BorderPanel {
      layout(textpane) = Center
      layout(buttonbar) = North
    } 
  
     peer.setLocationRelativeTo(null)
     
     def saveFile() = {
         val w = new BufferedWriter(new FileWriter(file.get.getAbsoluteFile()))
         w.write(textArea.text)
         w.close()
         changed=false
         filename = file.get.getName()
         makeTitle()
    }
    
    def saveFileAs() {
         val chooser = if (file.isDefined) new FileChooser(file.get) else new FileChooser() 
         chooser.title = "Save as"
         val result = chooser.showSaveDialog(null)
         if (result == FileChooser.Result.Approve) {
               file = Some(chooser.selectedFile)
               val w = new BufferedWriter(new FileWriter(file.get.getAbsoluteFile()))
               w.write(textArea.text)
               w.close()
              changed=false
              filename = file.get.getName()
              makeTitle()
         }    
    }
    
    def checkSave(): Boolean = {
        val res = Dialog.showConfirmation(contents.head, 
				      "Do you want to save changes?", 
				      optionType=Dialog.Options.YesNoCancel,
				      title=title)
         if (res == Dialog.Result.Ok) {
           if (loaded) saveFile() else saveFileAs()
         }
        return (res != Dialog.Result.Cancel) 
    }
    
    def loadFile() {
         if (changed) { if (!checkSave()) return }
         val chooser = new FileChooser(new File("./data"))
         chooser.title = "Select file"
         val result = chooser.showOpenDialog(null)
         if (result == FileChooser.Result.Approve) {
             //actdir = new File(chooser.selectedFile.getAbsolutePath()) 
             file = Some(chooser.selectedFile)
             loaded = true
             textArea.text = Source.fromFile(file.get).mkString
             changed=false
             filename = file.get.getName()
             makeTitle()
         }    
    }
    
    def newFile() {
       if (changed) { if (!checkSave()) return }
       loaded = false
       textArea.text = ""  
       changed=false
       filename = "None"
       makeTitle()         
    }

    menuBar = new MenuBar {
         contents += new Menu("File") {
            mnemonic = Key.F 
            contents += new MenuItem(Action("Load file") {
              loadFile()
            }) {
              mnemonic = Key.L
            }
            contents += new MenuItem(Action("Save file") {
              if (loaded) saveFile() else saveFileAs() 
            }) {
              mnemonic = Key.S
            }
            contents += new MenuItem(Action("Save file As...") {
              saveFileAs()
            }) {
              mnemonic = Key.A
            }
            contents += new MenuItem(Action("New") {
              newFile()
            }) {
              mnemonic = Key.N
            }
            contents += new Separator()
            contents += new MenuItem(Action("Exit") {
              closeOperation() 
            }) {
              mnemonic = Key.E
            }
       }   
  
    } 
  
    
   def play() {
     Midi.openMidiOut() 
  
     val e = EitzEventListParser(textArea.text)
     
     if (e.isDefined) { 
        val f = new InstrumentEvent(GeneralMidi.Instrument(instrument)) ++
           e.get
        Midi.play(f.fixAt(0),bpm)
     } else {
       Dialog.showMessage(contents.head, "Syntax error")
     } 
   }
    
   def stop() {
     Midi.stopPlaying
   }
   
   def setSpeed(speed: Int) {
     bpm = speed
     Midi.changeSpeed(bpm)
   }
   
   def setInstrument(ins: String) {
     instrument = ins
     Midi.changeInstrument(GeneralMidi.Instrument(ins))
   }
    
    listenTo(playbutton, stopbutton, textArea, instruments.selection, speedslider)
    
    reactions += {
     case ButtonClicked(`playbutton`) => play()
     case ButtonClicked(`stopbutton`) => stop()   
     case ValueChanged(`textArea`) => {
           changed=true
           makeTitle()
       }
     case ValueChanged(`speedslider`) => setSpeed(speedslider.value)    
     case SelectionChanged( `instruments`) => setInstrument(instruments.selection.item)
    }
    
    override def closeOperation() { if (changed) { if (checkSave()) sys.exit(0) } else sys.exit(0) }
   
 /*   println( Midi.listOutputDevices )
    Midi.openMidiOut()
    Midi.loadSoundBank("c:/soundfont/Jeux14.SF2") */
  } 
}