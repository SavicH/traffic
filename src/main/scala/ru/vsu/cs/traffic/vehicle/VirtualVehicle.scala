package ru.vsu.cs.traffic.vehicle

import ru.vsu.cs.traffic.Direction.FORWARD
import ru.vsu.cs.traffic.{Point, TrafficFlow, TrafficModel, Vehicle}

class VirtualVehicle(flow: TrafficFlow, point: Point, offset: Double = 0)
  extends Vehicle {

  override private[traffic] def act(timeStep: Double): Unit = {}

  override def distance: Double = point -- flow.start + offset

  override def lane: Int = 0

  override def speed: Double = 0

  override def acceleration: Double = 0

  override def direction = FORWARD

  override def trafficFlow: TrafficFlow = flow

  override val length: Double = 0

  override private[traffic] val model: TrafficModel = null

  override protected def onReceive(message: Any): Unit = throw new UnsupportedOperationException()
}

object VirtualVehicle {
  def apply(flow: TrafficFlow, point: Point, offset: Double = 0) = {
    new VirtualVehicle(flow, point, offset)
  }
}