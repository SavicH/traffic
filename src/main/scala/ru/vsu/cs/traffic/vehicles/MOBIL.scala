package ru.vsu.cs.traffic.vehicles

import ru.vsu.cs.traffic.Vehicle

class MOBIL (private val vehicle: Vehicle) {

  private val p = vehicle.politenessFactor

  private val b = vehicle.maximumSafeDeceleration

  private val a = vehicle.thresholdAcceleration

  private def safetyCriterion(lane: Int): Boolean = false

  private def incentiveCriterion(lane: Int): Boolean = false

  def lane: Int = {
    List(vehicle.lane - 1, vehicle.lane + 1)
      .filter(i => i > 0 && i < vehicle.trafficFlow.lanes)
      .map(i => (safetyCriterion(i) && incentiveCriterion(i), i))
      .find(_._1 == true)
      .getOrElse((true, vehicle.lane))
      ._2
  }
}

object MOBIL {
  def apply(vehicle: Vehicle) = new MOBIL(vehicle)
}
