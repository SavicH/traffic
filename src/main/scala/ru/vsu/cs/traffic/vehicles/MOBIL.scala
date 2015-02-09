package ru.vsu.cs.traffic.vehicles

import ru.vsu.cs.traffic.Vehicle

class MOBIL (vehicle: Vehicle) {

  private val p = vehicle.politenessFactor

  private val b = vehicle.maximumSafeDeceleration

  private val a = vehicle.thresholdAcceleration

  def lane = vehicle.lane
}

object MOBIL {
  def apply(vehicle: Vehicle) = new MOBIL(vehicle)
}
