package musica
import musica.math._

package object symbol {

    implicit def intToSymbolicTime(x: Int) = SymbolicTime(x,1)
    
    implicit def RationalToSymbolicTime(r: Rational) = SymbolicTime(r)

}