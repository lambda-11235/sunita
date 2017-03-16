
package sunita


object SIUnitSystem extends BaseUnitSystem {
  val meter, m = newBaseUnit("meter", "m")
  val kilogram, kg = newBaseUnit("kilogram", "kg")
  val second, s = newBaseUnit("second", "s")
  val kelvin, K = newBaseUnit("kelvin", "K")
  val ampere, A = newBaseUnit("ampere", "A")
  val mole, mol = newBaseUnit("mole", "mol")
  val candela, cd = newBaseUnit("candela", "cd")

  val hertz, Hz = zeroUnit/s
  val newton, N = m * kg / s.pow(2)
  val pascal, Pa = N / m.pow(2)
  // TODO: Add all the named derived units
}
