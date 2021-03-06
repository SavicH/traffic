package ru.vsu.cs.traffic.vehicle

import ru.vsu.cs.traffic.Vehicle

trait IDMVehicle extends Vehicle {

  val desiredSpeed: Double = 16.7

  val timeHeadway: Double = 2

  val normalAcceleration: Double = 1.5

  val brakeDeceleration: Double = 1.5

  val minimalGap: Double = 1

  val accelerationExponent: Double = 4.0

  val idm = IDM(this)

  private[vehicle] def headVehicle(lane: Int = lane): Vehicle
}
