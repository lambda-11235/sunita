# Sunita

The units package is a package for doing dimensional analysis in Scala. While
other unit analysis packages like [squants](https://github.com/garyKeorkunian/squants)
may provide a more complete dimensional analysis framework, sunita was meant to
handle arbitrarily complex dimensional formulas. It does not aim to support all
possible units. For example, sunita can compute `(second * second)/second`,
while squants cannot (to the best of my knowledge).

Example usage:
```scala
import sunita.SI

import SI.unitSys._
import SI.Implicits._

scala> meter/newton
res0: sunita.SI.unitSys.Unit = kg^-1 s^2

scala> 2*meter/newton
res1: sunita.SI.Measurement = 2.0 kg^-1 s^2

scala> 2*meter*kilogram*newton
res2: sunita.SI.Measurement = 2.0 m^2 kg^2 s^-2

scala> res2.sqrt
res3: sunita.SI.Measurement = 1.4142135623730951 m kg s^-1
```
