
package units

import scala.language.implicitConversions

import units.unit._

package object quantity {

  class QuantityException(message: String) extends RuntimeException(message) {}

  /**
   * Represents a quantity, which is a unit multiplied by a coefficient.
   */
  case class Quantity[U <: Unit[U]](coeff: Double, unit: U) {

    def +(other: Quantity[U]): Quantity[U] = {
      if (unit != other.unit)
        throw new QuantityException("Mismatched units in +")
      else
        Quantity(coeff + other.coeff, unit)
    }

    def +(other: Double): Quantity[U] = {
      if (! unit.isUnitless)
        throw new QuantityException("Mismatched units in +")
      else
        Quantity(coeff + other, unit)
    }

    def -(other: Quantity[U]): Quantity[U] = {
      if (unit != other.unit)
        throw new QuantityException("Mismatched units in -")
      else
        Quantity(coeff - other.coeff, unit)
    }

    def -(other: Double): Quantity[U] = {
      if (! unit.isUnitless)
        throw new QuantityException("Mismatched units in -")
      else
        Quantity(coeff - other, unit)
    }

    def *(other: Quantity[U]): Quantity[U] =
      Quantity(coeff * other.coeff, unit * other.unit)

    def *(other: Double): Quantity[U] =
      Quantity(coeff * other, unit)

    def /(other: Quantity[U]): Quantity[U] =
      Quantity(coeff / other.coeff, unit / other.unit)

    def /(other: Double): Quantity[U] =
      Quantity(coeff / other, unit)

    def unary_+ : Quantity[U] = this

    def unary_- : Quantity[U] = Quantity(-coeff, unit)

    def <(other: Quantity[U]): Boolean = {
      if (unit != other.unit)
        throw new QuantityException("Cannot compare quantities with different units")
      else
        coeff < other.coeff
    }

    def <=(other: Quantity[U]): Boolean = (this == other) || (this < other)

    def >(other: Quantity[U]): Boolean = {
      if (unit != other.unit)
        throw new QuantityException("Cannot compare quantities with different units")
      else
        coeff > other.coeff
    }

    def >=(other: Quantity[U]): Boolean = (this == other) || (this > other)

    /**
     * Raises this quantity to the nth power.
     */
    def pow(n: Int): Quantity[U] = Quantity(math.pow(coeff, n), unit.pow(n))

    /**
     * Tries to take the nth root of a quantity.
     *
     * @param n The root to take (2 for the square root).
     *
     * @return The resulting quantity if possible, otherwise None.
     */
    def nroot(n: Int): Quantity[U] = unit.nroot(n) match {
      case None => throw new QuantityException("Can't take "
                                               + n.toString
                                               + " root of unit "
                                               + unit.toString)
      case Some(u) => {
        val c = math.pow(coeff, 1/n.toDouble)
        Quantity(c, u)
      }
    }

    /**
     * Takes the square root of this unit if possible, otherwise it returns
     * None.
     */
    def sqrt: Quantity[U] = nroot(2)

    /**
     * Maps a function to the coefficient of this unit.
     */
    def map(f: Double => Double): Quantity[U] = Quantity(f(coeff), unit)

    override def toString = {
      if (unit.isUnitless)
        coeff.toString
      else
        coeff.toString + " " + unit.toString
    }
  }

  /**
   * Constructs multiplicativ operations between a double and quantity.
   */
  implicit class Ops(lhs: Double) {
    def +[U <: Unit[U]](rhs: Quantity[U]) = rhs + lhs
    def -[U <: Unit[U]](rhs: Quantity[U]) = (-rhs) + lhs
    def *[U <: Unit[U]](rhs: Quantity[U]) = rhs * lhs
    def /[U <: Unit[U]](rhs: Quantity[U]) = rhs.pow(-1) * lhs
  }

}
