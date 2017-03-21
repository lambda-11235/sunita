
package sunita


object SIUnitSystem extends BaseUnitSystem {
  // Base units
  val meter, m = newBaseUnit("meter", "m")
  val kilogram, kg = newBaseUnit("kilogram", "kg")
  val second, s = newBaseUnit("second", "s")
  val kelvin, K = newBaseUnit("kelvin", "K")
  val ampere, A = newBaseUnit("ampere", "A")
  val mole, mol = newBaseUnit("mole", "mol")
  val candela, cd = newBaseUnit("candela", "cd")

  // Derived units
  val hertz, Hz = zeroUnit / s
  val newton, N = m * kg / s.pow(2)
  val pascal, Pa = N / m.pow(2)
  val joule, J = N * m
  val watt, W = J / s
  val coulomb, C = s * A
  val volt, V = W / A
  val farad, F = C / V
  val ohm, Ω = V / A
  val siemens, S = A / V
  val weber, Wb = V * s
  val tesla, T = Wb / m.pow(2)
  val henry, H = Wb / A
  val lumen, lm = cd
  val lux, lx = lm / m.pow(2)
  val becquerel, Bq = zeroUnit / s
  val gray, Gy = J / kg
  val sievert, Sv = J / kg
  val katal, kat = mol / s
}
