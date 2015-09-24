
package units.unitsystem

/**
 * A trait for all unit systems.
 */
trait UnitSystem[U <: UnitSystem[U]] {
  def isUnitless: Boolean

  def ==(other: U): Boolean
  def *(other: U): U
  def /(other: U): U
  def pow(n: Int): U

  def !=(other: U): Boolean = !(this == other)
}
