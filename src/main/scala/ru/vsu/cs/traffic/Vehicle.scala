package ru.vsu.cs.traffic

import ru.vsu.cs.traffic.lines.distance2point
import ru.vsu.cs.traffic.vehicles.VehicleImpl

trait Vehicle extends TrafficActor {

  private[traffic] def act(timeStep: Double): Unit

  def trafficFlow: TrafficFlow

  def distance: Double

  def location: Point = distance2point(distance, trafficFlow.start, trafficFlow.end)

  def lane: Int

  def speed: Double

  def acceleration: Double

  val length: Double
  
  val maneuverSpeed = 3

  override def toString = s"Vehicle(length=$length, distance=$distance, location=$location, lane=$lane, speed=$speed, acceleration=$acceleration, trafficFlow=$trafficFlow)"
}

object Vehicle {
  def apply(model: TrafficModel, trafficFlow: TrafficFlow) = {
    //TypedActor(model.actorSystem).typedActorOf(TypedProps(classOf[Vehicle], new VehicleImpl(trafficFlow)))
    new VehicleImpl(trafficFlow)
  }
}