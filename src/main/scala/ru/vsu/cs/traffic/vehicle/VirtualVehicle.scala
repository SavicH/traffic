package ru.vsu.cs.traffic.vehicle

import ru.vsu.cs.traffic.{Point, TrafficFlow, Vehicle}

class VirtualVehicle(flow: TrafficFlow, point: Point, offset: Double = 0)
  extends Vehicle {

  override private[traffic] def act(timeStep: Double): Unit = {}

  override def distance: Double = point -- flow.start + offset

  override def lane: Int = 0

  override def speed: Double = 0

  override def acceleration: Double = 0

  override def trafficFlow: TrafficFlow = flow

  override val length: Double = 0
}

object VirtualVehicle {
  def apply(flow: TrafficFlow, point: Point, offset: Double = 0) = {
    new VirtualVehicle(flow, point, offset)
  }
}