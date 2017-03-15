
package sunita

private[sunita] object Util {
  def ipow(x: Int, n: Int): Int = {
    if(n == 0) {
      1
    } else if(n < 0) {
      throw new RuntimeException("Can't raise integers to negative powers")
    } else if(n % 2 == 0) {
      val y = ipow(x, n/2)
      y*y
    } else {
      x * ipow(x, n - 1)
    }
  }

  def ipow(x: Double, n: Int): Double = {
    if(n == 0) {
      1.0
    } else if(n < 0) {
      1.0/ipow(x, -n)
    } else if(n % 2 == 0) {
      val y = ipow(x, n/2)
      y*y
    } else {
      x * ipow(x, n - 1)
    }
  }
}
