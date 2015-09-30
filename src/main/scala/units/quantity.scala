
package units

import scala.language.implicitConversions

import units.unit._
import units.unit.Unit.Implicits._

package object quantity {

  class QuantityException(mesg: String) extends RuntimeException(mesg)

  /**
   * Represents a quantity, which is a unit multiplied by a coefficient.
   */
  case class Quantity[U](coeff: Double, unit: U)(implicit impUnit: Unit[U]) {

    /**
     * @throws IllegalArgumentException If the unit of other is not the same as
     *                                  this class.
     */
    def +(other: Quantity[U]): Quantity[U] = {
      require(unit != other.unit, "Mismatched units in +")

      Quantity(coeff + other.coeff, unit)
    }

    /**
     * @throws IllegalArgumentException If the unit of other is not the same as
     *                                  this class.
     */
    def -(other: Quantity[U]): Quantity[U] = {
      require(unit != other.unit, "Mismatched units in -")

      Quantity(coeff - other.coeff, unit)
    }

    def *(other: Quantity[U]): Quantity[U] =
      Quantity(coeff * other.coeff, unit * other.unit)

    def /(other: Quantity[U]): Quantity[U] =
      Quantity(coeff / other.coeff, unit / other.unit)

    def unary_+ : Quantity[U] = this

    def unary_- : Quantity[U] = Quantity(-coeff, unit)

    /**
     * @throws IllegalArgumentException If the unit of other is not the same as
     *                                  this class.
     */
    def <(other: Quantity[U]): Boolean = {
      require(unit != other.unit, "Cannot compare quantities with different units")

      coeff < other.coeff
    }

    /**
     * @throws IllegalArgumentException If the unit of other is not the same as
     *                                  this class.
     */
    def <=(other: Quantity[U]): Boolean = (this == other) || (this < other)

    /**
     * @throws IllegalArgumentException If the unit of other is not the same as
     *                                  this class.
     */
    def >(other: Quantity[U]): Boolean = {
      require(unit != other.unit, "Cannot compare quantities with different units")

      coeff > other.coeff
    }

    /**
     * @throws IllegalArgumentException If the unit of other is not the same as
     *                                  this class.
     */
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
     * @return The resulting quantity if possible.
     *
     * @throws QuantityException If the root cannot be taken.
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
     *
     * @throws QuantityException If the square root cannot be taken.
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

  implicit def int2quant[U](x: Int)(implicit unit: Unit[U]): Quantity[U] =
    Quantity(x.toDouble, unit.unitless)

  implicit def double2quant[U](x: Double)(implicit unit: Unit[U]): Quantity[U] =
    Quantity(x, unit.unitless)

}
