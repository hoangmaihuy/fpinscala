package calculator

object Polynomial extends PolynomialInterface:
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] =
    Signal.apply {
      Math.pow(b(), 2.0) - 4.0 * a() * c()
    }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] =
    Signal.apply {
      if (delta() < 0) then
        Set()
      else
        Set(
          (-b() - Math.pow(delta(), 0.5)) / (2 * a()),
          (-b() + Math.pow(delta(), 0.5)) / (2 * a()),
        )
    }
