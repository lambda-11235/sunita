
package units.unit

/**
 * A trait that all unit systems should extend.
 */
trait Unit[U <: Unit[U]] {
  /** Returns true is this unit has no dimension. */
  def isUnitless: Boolean

  def ==(other: U): Boolean
  def *(other: U): U
  def /(other: U): U
  def pow(n: Int): U

  def !=(other: U): Boolean = !(this == other)
}
