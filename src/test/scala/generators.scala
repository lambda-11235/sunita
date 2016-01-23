
package sunita.test

import sunita.quantity._
import sunita.siunits._
import sunita.unit

import org.scalacheck._
import Shrink._



package object generators {

  implicit def arbSIUnit: Arbitrary[SIUnit] = Arbitrary {
    for {m <- Arbitrary.arbitrary[Int]
         kg <- Arbitrary.arbitrary[Int]
         s <- Arbitrary.arbitrary[Int]
         a <- Arbitrary.arbitrary[Int]
         k <- Arbitrary.arbitrary[Int]
         mol <- Arbitrary.arbitrary[Int]
         cd <- Arbitrary.arbitrary[Int]}
    yield SIUnit(m, kg, s, a, k, mol, cd)
  }

  implicit def shrinkSIUnit: Shrink[SIUnit] = Shrink { unit =>
    (for(m <- shrink(unit.m)) yield unit.copy(m = m)) append
    (for(kg <- shrink(unit.kg)) yield unit.copy(kg = kg)) append
    (for(s <- shrink(unit.s)) yield unit.copy(s = s)) append
    (for(a <- shrink(unit.a)) yield unit.copy(a = a)) append
    (for(k <- shrink(unit.k)) yield unit.copy(k = k)) append
    (for(mol <- shrink(unit.mol)) yield unit.copy(mol = mol)) append
    (for(cd <- shrink(unit.cd)) yield unit.copy(cd = cd))
  }



  implicit def arbQuant[U: unit.Unit](implicit arbUnit: Arbitrary[U]):
  Arbitrary[Quantity[U]] = Arbitrary {
    for {coeff <- Arbitrary.arbitrary[Double]
         unit <- Arbitrary.arbitrary[U]}
    yield Quantity(coeff, unit)
  }

  implicit def shrinkQuant[U: unit.Unit](implicit su: Shrink[U]):
  Shrink[Quantity[U]] = Shrink { quant =>
    val qs1 = for(coeff <- shrink(quant.coeff)) yield Quantity(coeff, quant.unit)

    val qs2 = for(unit <- shrink(quant.unit)) yield Quantity(quant.coeff, unit)

    qs1 append qs2
  }

}
