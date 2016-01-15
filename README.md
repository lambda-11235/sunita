======
Sunita
======

The units package is a package for doing dimensional analysis in Scala. While
other unit analysis packages like [squants](https://github.com/garyKeorkunian/squants)
may provide a more complete dimensional analysis framework, sunita was meant to
handle arbitrarily complex dimensional formulas. It does not aim to support all
possible units. For example, sunita can compute `(second * second)/second`,
while squants cannot (to the best of my knowledge).

Example usage:
```scala
import units.siunits._
import units.constants._

scala> 2*meter * gravConst
res0: units.quantity.Quantity[units.siunits.SIUnit] = 1.334856E-10 m^4 kg^-1 s^-2

scala> pascal == (newton / meter.pow(2))
res1: Boolean = true
```
