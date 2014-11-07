package musica

package object math {
    implicit class IntHelperRational(val i: Int) extends AnyVal {
       def \(j: Int) = Rational(i,j)
    }
    implicit def intToRational(x: Int) = Rational(x,1)

}