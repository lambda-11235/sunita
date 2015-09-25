
package units

import scala.language.implicitConversions

import units.unitsystem._

package object quantity {

  class QuantityException(message: String) extends RuntimeException(message) {}

  /**
   * Represents a quantity, which is a unit multiplied by a coefficient.
   */
  case class Quantity[U <: UnitSystem[U]](coeff: Double, unit: U) {

    def +(other: Quantity[U]): Quantity[U] = {
      if (unit != other.unit)
        throw new QuantityException("Mismatched units in +")
      else
        Quantity(coeff + other.coeff, unit)
    }

    def -(other: Quantity[U]): Quantity[U] = {
      if (unit != other.unit)
        throw new QuantityException("Mismatched units in -")
      else
        Quantity(coeff - other.coeff, unit)
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

    def pow(n: Int): Quantity[U] = Quantity(math.pow(coeff, n), unit.pow(n))

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

  implicit class Ops(lhs: Double) {
    def *[U <: UnitSystem[U]](rhs: Quantity[U]) = rhs * lhs
    def /[U <: UnitSystem[U]](rhs: Quantity[U]) = rhs.pow(-1) * lhs
  }

}
