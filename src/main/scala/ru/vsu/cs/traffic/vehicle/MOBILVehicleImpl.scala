package ru.vsu.cs.traffic.vehicle

import ru.vsu.cs.traffic._
import ru.vsu.cs.traffic.event._

class MOBILVehicleImpl(tf: TrafficFlow, m: TrafficModel, l: Int)
  extends IDMVehicleImpl(tf, m, l) with MOBILVehicle {

  private[vehicle] def backVehicle(lane: Int = lane): Vehicle = {
    val vehiclesMap = _trafficFlow.vehicles.filter(_.lane == lane)
      .map(v => (v.distance, v)).toMap
    val distances = vehiclesMap.keys.filter(_ < distance)
    if (distances.isEmpty) trafficFlow.virtualStart else vehiclesMap(distances.max).asInstanceOf[IDMVehicle]
  }

  override protected def basicMovement(timeStep: Double): Unit = {
    val newLane = mobil.lane
    if (newLane != lane) {
      model.fireVehicleEvent(LaneChanged(this, _lane))
      _lane = newLane
    }
    super.basicMovement(timeStep)
  }
}

