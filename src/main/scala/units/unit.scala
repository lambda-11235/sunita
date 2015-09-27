
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

  /**
   * Tries to take the nth root of a unit.
   *
  * @param n The root to take (2 for the square root).
  *
  * @return The resulting unit if possible, otherwise None.
   */
  def nroot(n: Int): Option[U]

  def !=(other: U): Boolean = !(this == other)
}
