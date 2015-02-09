package ru.vsu.cs.traffic

import ru.vsu.cs.traffic.util.line
import ru.vsu.cs.traffic.vehicles.VehicleImpl

trait Vehicle extends TrafficActor {

  val desiredSpeed: Double = 16.7

  val timeHeadway: Double = 2

  val normalAcceleration: Double = 1.5

  val brakeDeceleration: Double = 1.5

  val minimumGap: Double = 1

  val accelerationExponent: Double = 4.0

  private[traffic] def act(timeStep: Double): Unit

  def trafficFlow: TrafficFlow

  def distance: Double

  def location: Point = line.distance2point(distance, trafficFlow.start, trafficFlow.end)

  def lane: Int

  def speed: Double

  def acceleration: Double

  val length: Double
}

object Vehicle {
  def apply(model: TrafficModel, trafficFlow: TrafficFlow) = {
    //TypedActor(model.actorSystem).typedActorOf(TypedProps(classOf[Vehicle], new VehicleImpl(trafficFlow)))
    new VehicleImpl(trafficFlow)
  }
}