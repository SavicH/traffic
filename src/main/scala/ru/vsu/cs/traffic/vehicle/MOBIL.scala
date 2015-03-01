package ru.vsu.cs.traffic.vehicle

import ru.vsu.cs.traffic.Direction.{BACK, LEFT, RIGHT}
import ru.vsu.cs.traffic.Vehicle

class MOBIL(vehicle: MOBILVehicle) {

  private def p = vehicle.politenessFactor

  private def b = vehicle.maximumSafeDeceleration

  private def a = vehicle.thresholdAcceleration

  private def safetyCriterion(head: Vehicle, back: Vehicle): Boolean = {
    val accelerationCriterion = back match {
      case _: VirtualVehicle => true
      case v: IDMVehicle => v.idm.acceleration(vehicle) > -b
    }
    accelerationCriterion &&
      head.distance - vehicle.distance > head.length &&
      vehicle.distance - back.distance > vehicle.length
  }

  private val DirectionBias = 2.0

  private def directionBias(lane: Int) = {
    if (lane > vehicle.lane && vehicle.direction == RIGHT) DirectionBias
    else if (lane < vehicle.lane && (vehicle.direction == LEFT || vehicle.direction == BACK)) DirectionBias
    else 0.0
  }

  private def incentiveCriterion(lane: Int, head: Vehicle, back: Vehicle): Boolean = {
    val left = vehicle.idm.acceleration(head) - vehicle.idm.acceleration + directionBias(lane)
    val dis = back match {
      case _: VirtualVehicle => 0
      case v: IDMVehicle => v.idm.acceleration - v.idm.acceleration(vehicle)
    }
    val right = p * dis + a
    left > 0 && left > right
  }

  def lane: Int = {
    List(vehicle.lane - 1, vehicle.lane + 1)
      .filter(i => i > 0 && i <= vehicle.trafficFlow.lanes)
      .map(i => (i, vehicle.headVehicle(i), vehicle.backVehicle(i)))
      .map(i => (safetyCriterion(i._2, i._3) && incentiveCriterion(i._1, i._2, i._3), i._1))
      .find(_._1 == true)
      .getOrElse((true, vehicle.lane))
      ._2
  }
}

object MOBIL {
  def apply(vehicle: MOBILVehicle) = new MOBIL(vehicle)
}
