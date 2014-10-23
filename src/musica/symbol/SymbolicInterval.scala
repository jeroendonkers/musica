package musica.symbol

trait SymbolicIntervalBase {
   val step: Int
   val dev: Int
   val size: Int
   
}

trait SymbolicNoteBase {
   val step: Int
   val dev: Int
   val octave: Int
   val chr: Int
   val octavesteps: Int
}

trait SymbolicInterval[I <: SymbolicIntervalBase, N <: SymbolicNoteBase] extends SymbolicIntervalBase {   

  def create(step: Int, dev: Int): I
  
  def add(s: I, t: I): I = {
    create(s.step + t.step, s.size + (t.size - create(s.step + t.step,0).size))
  }
  def add(t: I): I = {
    create(step + t.step, size + (t.size - create(step + t.step,0).size))
  }
  def subtract(s: I, t: I): I = {
        val ndev = if (s.step<t.step)  (t.size - create(s.step - t.step,0).size) - s.size
        else (s.size - t.size) - create(s.step - t.step,0).size
        create(s.step - t.step, ndev)
  }
  def subtract(t: I): I = {
        val ndev = if (step<t.step)  (t.size - create(step - t.step,0).size) - size
        else (size - t.size) - create(step - t.step,0).size
        create(step - t.step, ndev)
  }
  def negate(t: I):I = create(-t.step,t.dev) 
  def negate(): I = create(-step,dev)
  
    def +(that: I): I = 
      if (step<0 && that.step<0) negate(add(negate(that)))
      else if (that.step<0) subtract(negate(that)) 
      else if (this.step<0) subtract(that, negate())
      else add(that)
    def -(that: I): I = 
      if (this.step<0 && that.step<0) subtract(negate(that),negate())
      else if (that.step<0) add(negate(that))
      else if (this.step<0) negate(add(negate(),that))
      else subtract(that)
    def unary_- :I = create(-step,dev)
    
    def *(that: Int): I = 
       create(step * that, size * that.abs - create(step * that,0).size)
  
    def isEnharmonic(that: I): Boolean = this.size == that.size
   
    def equalTo(t: I): Boolean = (step == t.step) && (dev == t.dev)
        
}


trait SymbolicNote[N <: SymbolicNoteBase, I <: SymbolicIntervalBase] extends SymbolicNoteBase {   
   
   def create(step: Int, dev: Int, octave: Int): N
   def createInterval(step: Int, dev: Int): I

   
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
   
    def +(that: I): N = {
     if (that.step <0) subtract(-that.step,that.dev, that.size ) 
     else add(that.step, that.dev, that.size)   
    }
    
    def -(that: I): N = {
     if (that.step <0) add(-that.step,that.dev, that.size ) 
     else subtract(that.step, that.dev, that.size)   
    }
    

    def interval(that: N): I = {
      val newstep = that.step + that.octave*octavesteps - step - octave*octavesteps
      val chrdiv = that.chr - chr
      val bassize = createInterval(newstep,0).size 
      val dev = (if (newstep==0) chrdiv else chrdiv.abs) - bassize
      createInterval(newstep, dev)
    }

    def isEnharmonic(that: N): Boolean = this.chr == that.chr

    def normalize(): N = create(step,dev,0)
    
    def equalTo(t: N): Boolean = (step == t.step) && (dev == t.dev) && (octave == t.octave)
    
    
}  

