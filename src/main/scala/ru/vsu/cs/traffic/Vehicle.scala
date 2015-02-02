package ru.vsu.cs.traffic

import akka.actor.{TypedActor, TypedProps}
import ru.vsu.cs.traffic.util.line

trait Vehicle extends TrafficActor {

  private[traffic] def act(timeStep: Double): Unit

  def trafficFlow: TrafficFlow

  def distance: Double

  def location: Point = line.distance2point(distance, trafficFlow.start, trafficFlow.end)

  def lane: Int

  def velocity: Double

  def acceleration: Double

  val length: Double
}

object Vehicle {
  def apply(model: TrafficModel, trafficFlow: TrafficFlow) = {
    def apply(model: TrafficModel, trafficFlow: TrafficFlow): Vehicle = {
      TypedActor(model.actorSystem).typedActorOf(TypedProps(classOf[Vehicle], new VehicleImpl(trafficFlow)))
    }
  }
}

class VehicleImpl (private var _trafficFlow: TrafficFlow)
  extends Vehicle {

  //todo: proper initialization
  private var _distance = 0
  private var _velocity = 0
  private var _lane = 1
  private var _acceleration = 0

  val length = 5.0

  override private[traffic] def act(timeStep: Double): Unit = ???

  override def lane: Int = _lane

  override def distance: Double = _distance

  override def velocity: Double = _velocity

  override def trafficFlow: TrafficFlow = _trafficFlow

  override def acceleration: Double = _acceleration

}
