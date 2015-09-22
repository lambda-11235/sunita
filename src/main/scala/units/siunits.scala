
package units

import units.quantity._
import units.unitsystem._

package object siunits {

  /**
   * Class to represent an SI unit. The fields are the powers of the base units
   * that make up the current unit.
   */
  case class SIUnit(m: Int, kg: Int, s: Int, a: Int, k: Int,
                    mol: Int, cd: Int) {

    def *(other: SIUnit): SIUnit =
      SIUnit(m + other.m, kg + other.kg, s + other.s, a + other.a,
             k + other.k, mol + other.mol, cd + other.cd)

    def /(other: SIUnit): SIUnit =
      SIUnit(m - other.m, kg - other.kg, s - other.s, a - other.a,
             k - other.k, mol - other.mol, cd - other.cd)

    def pow(n: Int): SIUnit = SIUnit(n * m, n * kg, n * s, n * a, n * k,
                                     n * mol, n * cd)

    override def toString = {
      val powers = List(m, kg, s, a, k, mol, cd)
      val symbols = List("m", "kg", "s", "A", "K", "mol", "cd")

      def asStr(power: Int, symbol: String): String = {
        if(power == 1)
          symbol
        else
          symbol ++ "^" ++ power.toString
      }

      var combined = powers.zip(symbols)
      combined = combined.filter{ _._1 != 0 }
      val init = combined.init.map{ x => asStr(x._1, x._2) + " " } .foldLeft(""){_+_}

      val last = combined.last
      init + asStr(last._1, last._2)
    }
  }

  implicit object SIUnitSystem extends UnitSystem[SIUnit] {
    def unitless: SIUnit = SIUnit(0, 0, 0, 0, 0, 0, 0)

    def unitEq(a: SIUnit, b: SIUnit): Boolean = a == b
    def unitMul(a: SIUnit, b: SIUnit): SIUnit = a * b
    def unitDiv(a: SIUnit, b: SIUnit): SIUnit = a / b
    def unitPow(a: SIUnit, n: Int): SIUnit = a pow n
  }

  implicit object SIQuantityIsFractional extends Fractional[Quantity[SIUnit]] {
    def compare(x: Quantity[SIUnit], y: Quantity[SIUnit]): Int = {
      if(x == y)
        0
      else if(x < y)
        -1
      else
        1
    }

    def fromInt(x: Int): Quantity[SIUnit] = Quantity(x.toDouble, SIUnit(0,0,0,0,0,0,0))

    def plus(x: Quantity[SIUnit], y: Quantity[SIUnit]): Quantity[SIUnit] = x + y
    def minus(x: Quantity[SIUnit], y: Quantity[SIUnit]): Quantity[SIUnit] = x - y
    def times(x: Quantity[SIUnit], y: Quantity[SIUnit]): Quantity[SIUnit] = x * y
    def div(x: Quantity[SIUnit], y: Quantity[SIUnit]): Quantity[SIUnit] = x / y

    def negate(x: Quantity[SIUnit]): Quantity[SIUnit] = -x

    def toDouble(x: Quantity[SIUnit]): Double = x.coeff
    def toFloat(x: Quantity[SIUnit]): Float = x.coeff.toFloat
    def toInt(x: Quantity[SIUnit]): Int = x.coeff.toInt
    def toLong(x: Quantity[SIUnit]): Long = x.coeff.toLong
  }

  // SI prefixes
  def yotta[U](q: Quantity[U])(implicit usys: UnitSystem[U]) = q * 1e24
  def zetta[U](q: Quantity[U])(implicit usys: UnitSystem[U]) = q * 1e21
  def exa[U](q: Quantity[U])(implicit usys: UnitSystem[U]) = q * 1e18
  def peta[U](q: Quantity[U])(implicit usys: UnitSystem[U]) = q * 1e15
  def tera[U](q: Quantity[U])(implicit usys: UnitSystem[U]) = q * 1e12
  def giga[U](q: Quantity[U])(implicit usys: UnitSystem[U]) = q * 1e9
  def mega[U](q: Quantity[U])(implicit usys: UnitSystem[U]) = q * 1e6
  def kilo[U](q: Quantity[U])(implicit usys: UnitSystem[U]) = q * 1e3
  def hecto[U](q: Quantity[U])(implicit usys: UnitSystem[U]) = q * 1e2
  def deka[U](q: Quantity[U])(implicit usys: UnitSystem[U]) = q * 1e1

  def deci[U](q: Quantity[U])(implicit usys: UnitSystem[U]) = q * 1e-1
  def centi[U](q: Quantity[U])(implicit usys: UnitSystem[U]) = q * 1e-2
  def milli[U](q: Quantity[U])(implicit usys: UnitSystem[U]) = q * 1e-3
  def micro[U](q: Quantity[U])(implicit usys: UnitSystem[U]) = q * 1e-6
  def nano[U](q: Quantity[U])(implicit usys: UnitSystem[U]) = q * 1e-9
  def pico[U](q: Quantity[U])(implicit usys: UnitSystem[U]) = q * 1e-12
  def femto[U](q: Quantity[U])(implicit usys: UnitSystem[U]) = q * 1e-15
  def atto[U](q: Quantity[U])(implicit usys: UnitSystem[U]) = q * 1e-18
  def zepto[U](q: Quantity[U])(implicit usys: UnitSystem[U]) = q * 1e-21
  def yocto[U](q: Quantity[U])(implicit usys: UnitSystem[U]) = q * 1e-24

  // Base units
  val siUnitless = Quantity(1, SIUnit(0, 0, 0, 0, 0, 0, 0))
  val meter = Quantity(1, SIUnit(1, 0, 0, 0, 0, 0, 0))
  val kilogram = Quantity(1, SIUnit(0, 1, 0, 0, 0, 0, 0))
  val second = Quantity(1, SIUnit(0, 0, 1, 0, 0, 0, 0))
  val ampere = Quantity(1, SIUnit(0, 0, 0, 1, 0, 0, 0))
  val kelvin = Quantity(1, SIUnit(0, 0, 0, 0, 1, 0, 0))
  val mole = Quantity(1, SIUnit(0, 0, 0, 0, 0, 1, 0))
  val candela = Quantity(1, SIUnit(0, 0, 0, 0, 0, 0, 1))

  // Derived units

  val gram: Quantity[SIUnit] = kilogram/1000
  val radian = siUnitless
  val steradian = siUnitless
  val hertz = second pow (-1)
  val newton = meter * kilogram / (second pow 2)
  val pascal = newton / (meter pow 2)
  val joule = newton * meter
  val watt = joule / second
  val coulomb = ampere * second
  val volt = watt / ampere
  val farad = coulomb / volt
  val ohm = volt / ampere
  val siemens = ampere / volt
  val weber = volt * second
  val tesla = weber / (meter pow 2)
  val henry = weber / ampere
  val lumen = candela * steradian
  val lux = lumen / (meter pow 2)
  val becquerel = second pow (-1)
  val gray = joule / kilogram
  val sievert = joule / kilogram
  val katal = mole / second

  // Usefull unnamed SI units
  val siArea = meter pow 2
  val siVolume = meter pow 3
  val siElectricField = volt / meter
  val siSpecificEntropy = joule / (kilogram * kelvin)
  val siSpecificEnergy = joule / kilogram
  val siPermittivity = farad / meter
  val siPermeability = henry / meter

}
