
package sunita

import scala.language.implicitConversions


class MeasSystem[U <: UnitSystem](val unitSys: U) {
  type Coeff = Double


  sealed trait MeasError extends Error

  case object MismatchedUnits extends MeasError {
    override def toString: String = "Units are not the same, can't perform addition (subtraction)"
  }


  case class Measurement(coeff: Coeff, unit: unitSys.Unit) {
    def +(m: Measurement): Measurement = {
      if(unit == m.unit) {
        Measurement(coeff + m.coeff, unit)
      } else {
        throw MismatchedUnits
      }
    }


    def -(m: Measurement): Measurement = {
      if(unit == m.unit) {
        Measurement(coeff - m.coeff, unit)
      } else {
        throw MismatchedUnits
      }
    }


    def *(m: Measurement): Measurement = Measurement(coeff * m.coeff, unit * m.unit)

    def /(m: Measurement): Measurement = Measurement(coeff / m.coeff, unit / m.unit)

    def pow(n: Int): Measurement = Measurement(Util.ipow(coeff, n), unit.pow(n))

    def nroot(n: Int): Measurement = Measurement(math.pow(coeff, 1/n.toDouble), unit.nroot(n))

    def sqrt: Measurement = nroot(2)


    override def toString: String = coeff.toString + " " + unit.toString
  }


  object Implicits {
    implicit def intToMeas(x: Int): Measurement = doubleToMeas(x.toDouble)
    implicit def doubleToMeas(x: Double): Measurement = Measurement(x, unitSys.zeroUnit)
    implicit def unitToMeas(u: unitSys.Unit): Measurement = Measurement(1.0, u)

    implicit class MeasMakerInt(n: Int) {
      def *(u: unitSys.Unit): Measurement = intToMeas(n) * unitToMeas(u)
      def /(u: unitSys.Unit): Measurement = intToMeas(n) / unitToMeas(u)
    }

    implicit class MeasMakerDouble(x: Double) {
      def *(u: unitSys.Unit): Measurement = doubleToMeas(x) * unitToMeas(u)
      def /(u: unitSys.Unit): Measurement = doubleToMeas(x) / unitToMeas(u)
    }
  }
}
