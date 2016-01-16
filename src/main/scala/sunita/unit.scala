
package sunita.unit

/**
 * A trait that all unit systems should implicitly extend.
 */
trait Unit[U] {
  /** Returns true is this unit has no dimension. */
  def unitless: U

  def eq(lhs: U, rhs: U): Boolean

  def mult(lhs: U, rhs: U): U

  def div(lhs: U, rhs: U): U

  def pow(u: U, n: Int): U

  /**
   * Tries to take the nth root of a unit.
   *
  * @param n The root to take (2 for the square root).
  *
  * @return The resulting unit if possible, otherwise None.
   */
  def nroot(u: U, n: Int): Option[U]
}

object Unit {
  object Implicits {
    implicit class UnitOps[U](lhs: U)(implicit unit: Unit[U]) {
      def isUnitless = unit.eq(lhs, unit.unitless)

      def *(rhs: U): U = unit.mult(lhs, rhs)

      def /(rhs: U): U = unit.div(lhs, rhs)

      def pow(rhs: Int): U = unit.pow(lhs, rhs)

      def nroot(rhs: Int): Option[U] = unit.nroot(lhs, rhs)
    }
  }
}
