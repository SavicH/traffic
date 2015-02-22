package ru.vsu.cs.traffic.util

import ru.vsu.cs.traffic.Probability

import scala.math._

package object probability {

  def constant(const: Double): Probability = (x: Double) => const

  def exponent(expected: Double, variance: Double, module: Double): Probability = {
    (x: Double) => 1 / sqrt(2 * Pi) * exp(- (x % module - expected) / (2 * pow(variance, 2)))
  }

  def exponent(variance: Double): Probability = exponent(variance, variance, 2 * variance)
}
