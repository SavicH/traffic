package ru.vsu.cs.traffic.vehicle

import ru.vsu.cs.traffic.{TrafficFlow, TrafficModel}

class Truck(tf: TrafficFlow, m: TrafficModel, lane: Int) extends MOBILVehicleImpl(tf, m, lane) {

  override val length = 12.5

  override val desiredSpeed: Double = 50.0 / 3.6

  override val timeHeadway: Double = 2.5

  override val normalAcceleration: Double = 1.0

  override val brakeDeceleration: Double = 1.0

  override val minimalGap: Double = 2.0

  override val politenessFactor: Double = 0.4

  override val thresholdAcceleration: Double = 0.6

}
