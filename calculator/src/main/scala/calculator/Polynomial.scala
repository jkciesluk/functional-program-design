package calculator

object Polynomial extends PolynomialInterface {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(b()*b() - 4*a()*c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal(
      delta() match {
        case bg if bg > 0 => 
          val rt = Math.pow(bg, 0.5)
          val den = 2*a()
          Set((-b() -rt)/den, (-b() + rt)/den)
        case zero if zero == 0 => Set(-b()/(2*a()))
        case _ => Set()
      }
    )
  }
}
