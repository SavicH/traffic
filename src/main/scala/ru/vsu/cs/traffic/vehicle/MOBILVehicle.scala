package ru.vsu.cs.traffic.vehicle

import ru.vsu.cs.traffic.Vehicle

trait MOBILVehicle extends IDMVehicle {

  val politenessFactor: Double = 0.4

  val maximumSafeDeceleration: Double = 4.0

  val thresholdAcceleration: Double = 0.4

  def backVehicle(lane: Int = lane): Vehicle

  val mobil = MOBIL(this)
}
