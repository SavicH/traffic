package ru.vsu.cs.traffic.vehicles

class MOBIL(vehicle: MOBILVehicle) {

  private def p = vehicle.politenessFactor

  private def b = vehicle.maximumSafeDeceleration

  private def a = vehicle.thresholdAcceleration

  private def safetyCriterion(lane: Int): Boolean = {
    vehicle.backVehicle(lane) match {
      case _: VirtualVehicle => true
      case v: IDMVehicle => v.idm.acceleration(vehicle) > -b
    }
  }

  private def incentiveCriterion(lane: Int): Boolean = {
    val left = vehicle.idm.acceleration(vehicle.headVehicle(lane)) - vehicle.idm.acceleration
    val dis = vehicle.backVehicle(lane) match {
      case _: VirtualVehicle => 0
      case v: IDMVehicle => v.idm.acceleration - v.idm.acceleration(vehicle)
    }
    val right = p * dis + a
    left > right
  }

  def lane: Int = {
    List(vehicle.lane - 1, vehicle.lane + 1)
      .filter(i => i > 0 && i <= vehicle.trafficFlow.lanes)
      .map(i => (safetyCriterion(i) && incentiveCriterion(i), i))
      .find(_._1 == true)
      .getOrElse((true, vehicle.lane))
      ._2
  }
}

object MOBIL {
  def apply(vehicle: MOBILVehicle) = new MOBIL(vehicle)
}
