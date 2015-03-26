package ru.vsu.cs.traffic

import ru.vsu.cs.traffic.util.line
import ru.vsu.cs.traffic.vehicle.{SimpleVehicle, VehicleImpl}

trait Vehicle extends TrafficActor {

  private[traffic] def act(timeStep: Double): Unit

  def trafficFlow: TrafficFlow

  def distance: Double

  def location: Point = line.distance2point(distance, trafficFlow.start, trafficFlow.end)

  def lane: Int

  def speed: Double

  def acceleration: Double

  val length: Double
  
  val maneuverSpeed = 3

  def direction: Direction

  val timeToTurnRight: Double = 0.5

  val timeToTurnLeft: Double = 2

  val timeToTurnBack: Double = 2

  override def toString = s"Vehicle(length=$length, distance=$distance, location=$location, lane=$lane, speed=$speed, acceleration=$acceleration, trafficFlow=$trafficFlow)"
}

object Vehicle {
  def apply(model: TrafficModel, trafficFlow: TrafficFlow, lane: Int, head: Vehicle = null) = {
    if (model.isSimple)
      new SimpleVehicle(trafficFlow, model, head, lane)
    else
      new VehicleImpl(trafficFlow, model, lane)
  }
}