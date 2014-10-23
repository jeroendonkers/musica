package musica.symbol
/**
 * 
 *  Traits for implementing a system for symbolic intervals and notes
 *  the classic western system is implemented in ClassicNote and ClassicInterval
 * 
 * 
 *  Classes need to come in pairs (Notes and their intervals), so these traits are developed
 *  as a pair of interdependent traits, supported by basic traits.
 *  
 *  You need to extend SymbolicInterval and SymbolicNote and use both new classes in the type parameters.  
 *  
 *  Implement the constructor functions "create" and "createInterval" as indicated below
 *  
 *  implement all the indicated value fields in the base classes    
 *  You also must define the value "octavesteps" in the both classes
 *  
 *          
 */


// do not extend this trait
trait SymbolicIntervalBase {
   val step: Int      		// parameter in constructor
   val dev: Int  			// parameter in constructor
   
   val octavesteps: Int 	// implement this value!  how many steps is an octave (7 in classic)
   val octavesize: Int       // implement this value!  how big is the octave?  (12 in classic)
   def canbepure(st: Int): Boolean   // implement this value!  can this interval be pure (like pure fifth)?
   def basicsize(st: Int): Int       // implement this value!  size of the normalized interval
  
   lazy val normstep = step.abs % octavesteps  // steps of normalized interval
   lazy val octaves = step.abs / octavesteps   // number of octaves in interval
   lazy val size =  basicsize(normstep) + octavesize * (step.abs/octavesteps) + dev
   
   
}

// do not extend this trait
trait SymbolicNoteBase {
   val step: Int        // implement this value! (values only between 0 and octavesteps)
   val dev: Int         // implement this value! 
   val octave: Int      // implement this value! 
   
   val octavesteps: Int 	// implement this value!  how many steps is an octave (7 in classic)
   val octavesize: Int       // implement this value!  how big is the octave?  (12 in classic)
   def notepos(st: Int): Int  // implement this
    
   lazy val chr = notepos(step) + dev + octave*octavesize
   
   type NN <: SymbolicNoteBase
   def +(that: SymbolicIntervalBase): NN
   def -(that: SymbolicIntervalBase): NN
}

trait MidiCode extends SymbolicNoteBase {
  val midicode: Int
 }

// you have to extend this
trait SymbolicInterval[I <: SymbolicIntervalBase, N <: SymbolicNoteBase] extends SymbolicIntervalBase {   

  def create(step: Int, dev: Int): I // implement this constructor

  def normalize(): I = {
    if (step>=0) create(normstep, dev)
    else create((octavesteps-normstep) % octavesteps, -dev - (if (canbepure(normstep)) 0 else 1))
  }
  
  def invert(): I = subtract(create(octavesteps,0), normalize)
  
  def add(s: SymbolicIntervalBase, t: SymbolicIntervalBase): I = {
    create(s.step + t.step, s.size + (t.size - create(s.step + t.step,0).size))
  }
  def add(t: SymbolicIntervalBase): I = {
    create(step + t.step, size + (t.size - create(step + t.step,0).size))
  }
  def subtract(s: SymbolicIntervalBase, t: SymbolicIntervalBase): I = {
        val ndev = if (s.step<t.step)  (t.size - create(s.step - t.step,0).size) - s.size
        else (s.size - t.size) - create(s.step - t.step,0).size
        create(s.step - t.step, ndev)
  }
  def subtract(t: SymbolicIntervalBase): I = {
        val ndev = if (step<t.step)  (t.size - create(step - t.step,0).size) - size
        else (size - t.size) - create(step - t.step,0).size
        create(step - t.step, ndev)
  }
  def negate(t: SymbolicIntervalBase):I = create(-t.step,t.dev) 
  def negate(): I = create(-step,dev)
  
    def +(that: SymbolicIntervalBase): I = 
      if (step<0 && that.step<0) negate(add(negate(that)))
      else if (that.step<0) subtract(negate(that)) 
      else if (this.step<0) subtract(that, negate())
      else add(that)
    def -(that: SymbolicIntervalBase): I = 
      if (this.step<0 && that.step<0) subtract(negate(that),negate())
      else if (that.step<0) add(negate(that))
      else if (this.step<0) negate(add(negate(),that))
      else subtract(that)
    def unary_- :I = create(-step,dev)
    
    def *(that: Int): I = 
       create(step * that, size * that.abs - create(step * that,0).size)
  
    def isEnharmonic(that: I): Boolean = this.size == that.size
   
    def equalTo(t: SymbolicIntervalBase): Boolean = (step == t.step) && (dev == t.dev)
    
    def on(c: N) = c + this
    def below(c: N) = c - this
    
    def on(l: List[SymbolicNoteBase]) = l.map(c => c + this)
    def below(l: List[SymbolicNoteBase]) =  l.map(c => c - this)  
  
    
    override def equals(that: Any): Boolean = {
      that match {
       case that: SymbolicIntervalBase => equalTo(that)
       case _ => false
     }
   }
  
}


trait SymbolicNote[N <: SymbolicNoteBase, I <: SymbolicIntervalBase] extends SymbolicNoteBase {   
   
   type NN = N
     
   def create(step: Int, dev: Int, octave: Int): N  // implement this constructor
   def createInterval(step: Int, dev: Int): I       // implement this constructor

   def add(thatstep: Int, thatdev: Int, thatsize: Int): N = {
     val newstep = (step + thatstep) % octavesteps
     val newoctave = octave + (step + thatstep) / octavesteps
     
     val newdev = chr + thatsize - create(newstep,0,newoctave).chr
     create(newstep,newdev, newoctave)
   }
   
   def subtract(thatstep: Int, thatdev: Int, thatsize: Int): N = {
      val ns = thatstep % octavesteps
      val newstep = (step - ns + (if (ns>step) octavesteps else 0)) % octavesteps
      val newoctave = octave - (if (thatstep>step) (1+((thatstep-step-1) / octavesteps)) else 0)
      val newdev = if (ns == 0) (dev-thatdev) else ((chr - thatsize) - (create(newstep,0,newoctave).chr))
      create(newstep,newdev, newoctave)
   }
   
    def +(that: SymbolicIntervalBase): N = {
     if (that.step <0) subtract(-that.step,that.dev, that.size ) 
     else add(that.step, that.dev, that.size)   
    }
    
    def -(that: SymbolicIntervalBase): N = {
     if (that.step <0) add(-that.step,that.dev, that.size ) 
     else subtract(that.step, that.dev, that.size)   
    }
    

    def interval(that: SymbolicNoteBase): I = {
      val newstep = that.step + that.octave*octavesteps - step - octave*octavesteps
      val chrdiv = that.chr - chr
      val bassize = createInterval(newstep,0).size 
      val dev = (if (newstep==0) chrdiv else chrdiv.abs) - bassize
      createInterval(newstep, dev)
    }

    def isEnharmonic(that: N): Boolean = this.chr == that.chr

    def normalize(): N = create(step,dev,0)
    
    def equalTo(t: SymbolicNoteBase): Boolean = (step == t.step) && (dev == t.dev) && (octave == t.octave)
    
      override def equals(that: Any): Boolean = {
     that match {
       case that: SymbolicNoteBase=> equalTo(that)
       case _ => false
     }
   }
}  


class SymbolicNoteSequence[N <: SymbolicNoteBase](val notelist: List[N]) {
  val size = notelist.size
  override def toString(): String = notelist.toString
 }


abstract class SymbolicScale[N <: SymbolicNote[N,I],I <: SymbolicInterval[I,N]](val steplist: List[I]) {
   
   val prime: I  // implement this 
   val octave: I // implement this 
   
   val size = steplist.size 
    
    
   def this(stps: I*) { this(stps.toList) }
   def on(c: N) = new SymbolicNoteSequence[N](steplist.map(e => e.on(c)))
   def step(i: Int): I = {
     if (size==0) prime else {
       val numoctaves = if (i>=0) (i / size) else  ((i+1) / size) - 1  
       val idx = if (i>=0) (i % size) else (size-1- ((-i-1) % size))    
       (octave * numoctaves) + steplist(idx)
     }  
   }
   
   def stepNumber(c: I): Int = {
     steplist.indexOf(c)
   }
  
}
