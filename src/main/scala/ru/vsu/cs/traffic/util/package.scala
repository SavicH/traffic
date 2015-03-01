package ru.vsu.cs.traffic

package object util {

  def counter(timeStep: Double, currentTime: Double, minimalTime: Double)(action: => Unit): Double = {
    if (currentTime >= minimalTime) {
      action
      0
    } else {
      currentTime + timeStep
    }
  }
}
