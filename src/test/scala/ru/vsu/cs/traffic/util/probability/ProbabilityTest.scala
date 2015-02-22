package ru.vsu.cs.traffic.util.probability

import org.scalatest.FunSuite

class ProbabilityTest extends FunSuite {

  test("Const probability") {
    val prob = constant(0.2)
    assert(prob(1.0) == 0.2)
  }

  test("Exponent probability") {
    val prob = exponent(3, 1, 10)
    assert(prob(3.0) == 1 / math.sqrt(2 * math.Pi))
  }
}
