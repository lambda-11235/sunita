
package units

import scala.language.implicitConversions

import units.unitsystem._

package object quantity {

  class QuantityException(message: String) extends RuntimeException(message) {}

  /**
   * Represents a quantity, which is a unit multiplied by a coefficient.
   */
  case class Quantity[U](coeff: Double, unit: U)(implicit usys: UnitSystem[U]) {

    def +(other: Quantity[U]): Quantity[U] = {
      if(usys.unitNotEq(unit, other.unit))
        throw new QuantityException("Mismatched units in +")
      else
        Quantity(coeff + other.coeff, unit)
    }

    def -(other: Quantity[U]): Quantity[U] = {
      if(usys.unitNotEq(unit, other.unit))
        throw new QuantityException("Mismatched units in +")
      else
        Quantity(coeff - other.coeff, unit)
    }

    def *(other: Quantity[U]): Quantity[U] =
      Quantity(coeff * other.coeff, usys.unitMul(unit, other.unit))

    def /(other: Quantity[U]): Quantity[U] =
      Quantity(coeff / other.coeff, usys.unitDiv(unit, other.unit))

    def unary_+ : Quantity[U] = this

    def unary_- : Quantity[U] = Quantity(-coeff, unit)

    def <(other: Quantity[U]): Boolean = {
      if(unit != other.unit)
        throw new QuantityException("Cannot compare quantities with different units")
      else
        coeff < other.coeff
    }

    def <=(other: Quantity[U]): Boolean = (this == other) || (this < other)

    def >(other: Quantity[U]): Boolean = {
      if(unit != other.unit)
        throw new QuantityException("Cannot compare quantities with different units")
      else
        coeff > other.coeff
    }

    def >=(other: Quantity[U]): Boolean = (this == other) || (this > other)

    def pow(n: Int): Quantity[U] =
      Quantity(math.pow(coeff, n), usys.unitPow(unit, n))

    /**
     * Applies a function to the coefficient of this unit.
     */
    def applyCoeff(f: Double => Double): Quantity[U] = Quantity(f(coeff), unit)

    override def toString = {
      if(usys.unitEq(unit, usys.unitless))
        coeff.toString
      else
        coeff.toString + " " + unit.toString
    }
  }

  implicit def int2quant[U](i: Int)(implicit usys: UnitSystem[U]) =
    Quantity(i.toDouble, usys.unitless)

  implicit def double2quant[U](d: Double)(implicit usys: UnitSystem[U]) =
    Quantity(d, usys.unitless)

  implicit def unit2quant[U](u: U)(implicit usys: UnitSystem[U]) =
    Quantity(1, u)

  // Some useful quantities for all unit systems
  def pi[U](implicit usys: UnitSystem[U]): Quantity[U] = double2quant(math.Pi)

}
