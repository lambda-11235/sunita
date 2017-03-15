
package sunita

trait UnitSystem {
  type Unit <: UnitT
  type UnitError <: Error

  /**
   * The zero unit is a unit that when multiplied by any other unit produces
   * the other unit (`zeroUnit*u => u`).
   */
  def zeroUnit: Unit

  trait UnitT {
    def *(u: Unit): Unit
    def /(u: Unit): Unit

    def pow(n: Int): Unit

    /**
     * Takes the nth root of this unit.
     */
    def nroot(n: Int): Unit

    def sqrt: Unit = nroot(2)
  }
}
