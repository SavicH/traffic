package ru.vsu.cs.traffic.vehicles

import ru.vsu.cs.traffic.Vehicle

trait MOBILVehicle extends IDMVehicle {

  val politenessFactor: Double = 0.25

  val maximumSafeDeceleration: Double = 4.0

  val thresholdAcceleration: Double = 0.2

  def backVehicle(lane: Int = lane): Vehicle

  val mobil = MOBIL(this)
}
