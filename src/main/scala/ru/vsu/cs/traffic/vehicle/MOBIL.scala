package ru.vsu.cs.traffic.vehicle

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

  private def incentiveCriterion(head: Vehicle, back: Vehicle): Boolean = {
    val left = vehicle.idm.acceleration(head) - vehicle.idm.acceleration
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
      .map(i => (safetyCriterion(i._2, i._3) && incentiveCriterion(i._2, i._3), i._1))
      .find(_._1 == true)
      .getOrElse((true, vehicle.lane))
      ._2
  }
}

object MOBIL {
  def apply(vehicle: MOBILVehicle) = new MOBIL(vehicle)
}
