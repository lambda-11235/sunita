
package sunita


object SIUnitSystem extends BaseUnitSystem {
  val m = newBaseUnit("meter", "m")
  val kg = newBaseUnit("kilogram", "kg")
  val s = newBaseUnit("second", "s")
  val K = newBaseUnit("kelvin", "K")
  val A = newBaseUnit("ampere", "A")
  val mol = newBaseUnit("mole", "mol")
  val cd = newBaseUnit("candela", "cd")

  val Hz = zeroUnit/s
  val N = m * kg / s.pow(2)
  val Pa = N / m.pow(2)
  // TODO: Add all the named derived units
}
