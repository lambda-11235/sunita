
package units

import scala.math.Pi

import units.siunits._
import units.quantity._

/**
 * Various physical constants in SI units.
 */
object constants {

  val avogodrosConst = 6.02214179e23 / mole

  val speedOfLight = 2.99792458e8 * meter/second

  /** Atomic mass unit. */
  val amu = 1.660538782e-27 * kilogram
  val deutronMass = 3.34358320e-27 * kilogram
  val protonMass = 1.672621637e-27 * kilogram
  val neutronMass = 1.674927211e-27 * kilogram
  val electronMass = 9.10938215e-31 * kilogram
  val elemCharge = 1.602176487e-19 * coulomb
  val electronVolt = elemCharge * volt

  val gasConst = 8.314472 * joule/(mole*kelvin)

  val gravConst = 6.67428e-11 * newton*(meter pow 2)/(kilogram pow 2)

  val permeabilityOfFS = (4*Pi)*(1e-7) * tesla*meter/ampere
  val permittivityOfFS = 1/(permeabilityOfFS * (speedOfLight pow 2))
  val coulombConst = 1/(4*Pi * permittivityOfFS)

  /* ****** Earth Constants ******* */
  val earthRadius = 6.371e6 * meter
  val earthMass = 5.972e24 * kilogram

  /** Acceleration due to gravity on Earth. */
  val gee = 9.80665 * meter / (second pow 2)

}
