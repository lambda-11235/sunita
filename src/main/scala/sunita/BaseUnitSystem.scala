
package sunita

import scala.collection.immutable.Map


/**
 * A unit system composed of some base units.
 */
trait BaseUnitSystem extends UnitSystem {
  private[BaseUnitSystem] case class BaseUnit(name: String, sym: String)

  def zeroUnit: Unit = new Unit(Map())

  protected def newBaseUnit(name: String, sym: String) = {
    new Unit(Map(BaseUnit(name, sym) -> 1))
  }


  sealed trait UnitError extends Error

  case class NRootError(unit: Unit, n: Int) extends UnitError {
    override def toString: String = "Can't take the " + n.toString + " root of " + unit.toString
  }


  class Unit private[BaseUnitSystem](private val powers: Map[BaseUnit, Int]) extends UnitT {
    def *(u: Unit): Unit = combine(u, _+_)
    def /(u: Unit): Unit = combine(u, _-_)

    def pow(n: Int): Unit = new Unit(powers.map { case (u, p) => (u, n*p) })

    def nroot(n: Int): Unit = {
      if(powers.values.forall(_ % n == 0)) {
        new Unit(powers.map { case (u, p) => (u, p/n) })
      } else {
        throw NRootError(this, n)
      }
    }


    /**
     * Breaks this unit down into its base units and the powers to which they
     * are raised.
     */
    def inBaseUnitPowers: Map[Unit, Int] = powers.map { case (u, p) =>
      (new Unit(Map(u -> 1)), p)
    }


    private def combine(u: Unit, f: (Int, Int) => Int): Unit = {
      val baseUnits = powers.keys ++ u.powers.keys

      val newPowers = for(baseUnit <- baseUnits)
      yield (baseUnit, f(powers.getOrElse(baseUnit, 0), u.powers.getOrElse(baseUnit, 0)))

      new Unit(newPowers.filter(_._2 != 0).toMap)
    }


    override def toString: String = {
      val baseUnitParts = powers.map{ case (u, p) =>
        if(p != 1) {
          u.sym + "^" + p.toString
        } else {
          u.sym
        }
      }

      baseUnitParts.mkString("", " ", "")
    }
  }
}
