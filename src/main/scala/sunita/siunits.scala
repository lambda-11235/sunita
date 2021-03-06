
package sunita

import scala.language.implicitConversions

import sunita.quantity._
import sunita.unit._

package object siunits {

  /**
   * A convenient type synonym.
   */
  type SIQuant = Quantity[SIUnit]

  /**
   * Class to represent an SI unit. The fields are the powers of the base units
   * that make up the current unit.
   */
  case class SIUnit(m: Int, kg: Int, s: Int, a: Int, k: Int, mol: Int, cd: Int) {

    def applyToPowers(f: Int => Int) = SIUnit(f(m), f(kg), f(s), f(a),
                                                      f(k), f(mol), f(cd))

    def merge(other: SIUnit, joinFun: (Int, Int) => Int) = {
      SIUnit(joinFun(m, other.m),
             joinFun(kg, other.kg),
             joinFun(s, other.s),
             joinFun(a, other.a),
             joinFun(k, other.k),
             joinFun(mol, other.mol),
             joinFun(cd, other.cd))
    }

    def *(other: SIUnit): SIUnit = merge(other, ((x, y) => x + y))

    def /(other: SIUnit): SIUnit = merge(other, ((x, y) => x - y))

    def pow(n: Int): SIUnit = applyToPowers(x => n * x)


    /**
     * Tries to take the nth root of an SI unit.
     *
     * @param n The root to take (2 for the square root).
     *
     * @return The resulting unit if possible, otherwise None.
     */
    def nroot(n: Int): Option[SIUnit] = {
      var params = List(m, kg, s, a, k, mol, cd)

      if(params.forall(_ % n == 0)) {
        params = params map (_/n)
        Some(SIUnit(params(0), params(1), params(2), params(3), params(4),
                    params(5), params(6)))
      } else {
        None
      }
    }

    override def toString = this match {
      case hertz.unit => "Hz"
      case newton.unit => "N"
      case pascal.unit => "Pa"
      case joule.unit => "J"
      case watt.unit => "W"
      case coulomb.unit => "C"
      case volt.unit => "V"
      case farad.unit => "F"
      case ohm.unit => "ω"
      case siemens.unit => "S"
      case weber.unit => "Wb"
      case tesla.unit => "T"
      case henry.unit => "H"
      case lumen.unit => "lm"
      case lux.unit => "lx"
      case becquerel.unit => "Bq"
      case gray.unit => "Gy"
      case sievert.unit => "Sv"
      case katal.unit => "kat"
      case _ => {
        val powers = List(m, kg, s, a, k, mol, cd)
        val symbols = List("m", "kg", "s", "A", "K", "mol", "cd")

        def asStr(power: Int, symbol: String): String = {
          if (power == 1)
            symbol
          else
            symbol ++ "^" ++ power.toString
        }

        var combined = powers.zip(symbols)
        combined = combined filter (_._1 != 0)

        val strs = combined map (x => asStr(x._1, x._2))
        strs.mkString(" ")
      }
    }
  }

  implicit object SIUnitIsUnit extends Unit[SIUnit] {
    def unitless = SIUnit(0, 0, 0, 0, 0, 0, 0)

    def eq(lhs: SIUnit, rhs: SIUnit) = rhs == lhs

    def mult(lhs: SIUnit, rhs: SIUnit) = lhs * rhs

    def div(lhs: SIUnit, rhs: SIUnit) = lhs / rhs

    def pow(u: SIUnit, n: Int) = u.pow(n)

    def nroot(u: SIUnit, n: Int) = u.nroot(n)
  }

  // SI prefixes
  def yotta(q: Quantity[SIUnit]) = q * 1e24
  def zetta(q: Quantity[SIUnit]) = q * 1e21
  def exa(q: Quantity[SIUnit]) = q * 1e18
  def peta(q: Quantity[SIUnit]) = q * 1e15
  def tera(q: Quantity[SIUnit]) = q * 1e12
  def giga(q: Quantity[SIUnit]) = q * 1e9
  def mega(q: Quantity[SIUnit]) = q * 1e6
  def kilo(q: Quantity[SIUnit]) = q * 1e3
  def hecto(q: Quantity[SIUnit]) = q * 1e2
  def deka(q: Quantity[SIUnit]) = q * 1e1

  def deci(q: Quantity[SIUnit]) = q * 1e-1
  def centi(q: Quantity[SIUnit]) = q * 1e-2
  def milli(q: Quantity[SIUnit]) = q * 1e-3
  def micro(q: Quantity[SIUnit]) = q * 1e-6
  def nano(q: Quantity[SIUnit]) = q * 1e-9
  def pico(q: Quantity[SIUnit]) = q * 1e-12
  def femto(q: Quantity[SIUnit]) = q * 1e-15
  def atto(q: Quantity[SIUnit]) = q * 1e-18
  def zepto(q: Quantity[SIUnit]) = q * 1e-21
  def yocto(q: Quantity[SIUnit]) = q * 1e-24

  // Base units
  val siUnitless = Quantity(1, SIUnit(0, 0, 0, 0, 0, 0, 0))
  val meter = Quantity(1, SIUnit(1, 0, 0, 0, 0, 0, 0))
  val kilogram = Quantity(1, SIUnit(0, 1, 0, 0, 0, 0, 0))
  val second = Quantity(1, SIUnit(0, 0, 1, 0, 0, 0, 0))
  val ampere = Quantity(1, SIUnit(0, 0, 0, 1, 0, 0, 0))
  val kelvin = Quantity(1, SIUnit(0, 0, 0, 0, 1, 0, 0))
  val mole = Quantity(1, SIUnit(0, 0, 0, 0, 0, 1, 0))
  val candela = Quantity(1, SIUnit(0, 0, 0, 0, 0, 0, 1))

  // Base unit multiples
  val gram: Quantity[SIUnit] = kilogram/1000
  val minute = 60 * second
  val hour = 60 * minute
  val day = 24 * hour
  val leapYear = 366 * day
  val year = 365 * day

  // Derived units

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

  val liter = 0.001 * meter.pow(3)

  // Usefull unnamed SI units
  val siArea = meter pow 2
  val siVolume = meter pow 3
  val siElectricField = volt / meter
  val siSpecificEntropy = joule / (kilogram * kelvin)
  val siSpecificEnergy = joule / kilogram
  val siPermittivity = farad / meter
  val siPermeability = henry / meter

}
