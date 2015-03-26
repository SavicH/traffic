package ru.vsu.cs.traffic.event

import ru.vsu.cs.traffic.{Intersection, TrafficFlow, Vehicle}

trait VehicleEvent

case class VehicleSpawned(vehicle: Vehicle) extends VehicleEvent

case class VehicleRemoved(vehicle: Vehicle) extends VehicleEvent

case class IntersectionPassed(vehicle: Vehicle, intersection: Intersection) extends VehicleEvent

case class TrafficFlowChanged(vehicle: Vehicle, oldFlow: TrafficFlow) extends VehicleEvent

case class LaneChanged(vehicle: Vehicle, oldLane: Int) extends VehicleEvent

case class VehicleStopped(vehicle: Vehicle) extends VehicleEvent

case class VehicleMoved(vehicle: Vehicle) extends VehicleEvent

