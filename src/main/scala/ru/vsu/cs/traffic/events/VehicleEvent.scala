package ru.vsu.cs.traffic.events

import ru.vsu.cs.traffic.{Intersection, TrafficFlow, Vehicle}

trait VehicleEvent{}

case class VehicleSpawnedEvent(vehicle: Vehicle) extends VehicleEvent
case class VehiclePassedIntersectionEvent(vehicle: Vehicle, intersection: Intersection) extends VehicleEvent
case class VehicleChangedLane(vehicle: Vehicle, oldLane: Int) extends VehicleEvent

