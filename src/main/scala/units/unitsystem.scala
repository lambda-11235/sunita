
package units.unitsystem

/**
 * A trait for all unit systems.
 */
trait UnitSystem[U] {
  def unitless: U

  def unitEq(a: U, b: U): Boolean
  def unitMul(a: U, b: U): U
  def unitDiv(a: U, b: U): U
  def unitPow(a: U, n: Int): U

  def unitNotEq(a: U, b: U): Boolean = !unitEq(a, b)
}
