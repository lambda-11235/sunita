
import sunita.quantity._
import sunita.siunits._
import sunita.unit

import sunita.test.generators._

import org.scalacheck._
import Prop._



object SunitaTest extends Properties("Quantities") {
  property("Self Equality") = forAll { (quant: SIQuant) =>
    quant == quant
  }

  property("Comparing") = forAll { (a: SIQuant) =>
    all(a <= a,
        a >= a)
  }

  property("Addition and Multiplication") = forAll { (quant: SIQuant) =>
    all((quant + quant == 2 * quant),
        (quant + quant + quant == 3 * quant))
  }
}
