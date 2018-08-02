package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(b() * b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal{
      computeDelta(a, b, c)() match {
        case delta if delta < 0 => Set()
        case delta if delta == 0 => Set(-b()/(2*a()))
        case delta if delta > 0 => Set((-b() + math.sqrt(delta))/(2*a()), (-b() - math.sqrt(delta))/(2*a()))
      }
    }
  }
}
