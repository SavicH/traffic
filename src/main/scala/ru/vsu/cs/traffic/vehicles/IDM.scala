package ru.vsu.cs.traffic.vehicles

import ru.vsu.cs.traffic.Vehicle

import scala.math._

class IDM(vehicle: Vehicle) {
  private val a = vehicle.normalAcceleration
  private val b = vehicle.brakeDeceleration
  private val v0 = vehicle.desiredSpeed
  private val delta = vehicle.accelerationExponent
  private val T = vehicle.timeHeadway
  private val s0 = vehicle.minimumGap

  private def v = vehicle.speed

  private def dynamicGap(head: Vehicle) = {
    val dv = vehicle.speed - head.speed
    s0 + max(0, v * T + v * dv / (2 * sqrt(a * b)))
  }

  def acceleration(head: Vehicle) = {
    val ss = dynamicGap(head)
    val s = head.distance - vehicle.distance - head.length
    a * (1 - pow(v / v0, delta) - pow(ss / s, 2))
  }
}

object IDM {
  def apply(vehicle: Vehicle) = new IDM(vehicle)
}
