package ru.vsu.cs.traffic.vehicles

import ru.vsu.cs.traffic.Vehicle

import scala.math._

class IDM(vehicle: IDMVehicle) {
  private def a = vehicle.normalAcceleration
  private def b = vehicle.brakeDeceleration
  private def v0 = vehicle.desiredSpeed
  private def delta = vehicle.accelerationExponent
  private def T = vehicle.timeHeadway
  private def s0 = vehicle.minimumGap

  private def v = vehicle.speed

  private def dynamicGap(head: Vehicle) = {
    val dv = vehicle.speed - head.speed
    s0 + max(0, v * T + v * dv / (2 * sqrt(a * b)))
  }

  def acceleration(head: Vehicle): Double = {
    val ss = dynamicGap(head)
    val s = head.distance - vehicle.distance - head.length
    a * (1 - pow(v / v0, delta) - pow(ss / s, 2))
  }

  def acceleration: Double = acceleration(vehicle.headVehicle(vehicle.lane))
}

object IDM {
  def apply(vehicle: IDMVehicle) = new IDM(vehicle)
}
