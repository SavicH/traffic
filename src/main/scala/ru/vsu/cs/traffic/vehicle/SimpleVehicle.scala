package ru.vsu.cs.traffic.vehicle

import ru.vsu.cs.traffic.Color.RED
import ru.vsu.cs.traffic.{Direction, TrafficFlow, TrafficModel, Vehicle}

class SimpleVehicle(tf: TrafficFlow, model: TrafficModel, h: Vehicle, lane: Int) extends IDMVehicleImpl(tf, model, lane) {
  val head: Vehicle = if (h == null) trafficFlow.virtualEnd else h

  override def headVehicle(lane: Int): Vehicle = {
    val lights = trafficFlow.trafficLights
      .filter(l => l.color == RED && l.distance < head.distance && l.distance > distance)
    if (lights.nonEmpty) VirtualVehicle(trafficFlow, lights.reduceLeft((t1, t2) => if (t1.distance < t2.distance) t1 else t2).location)
    else if (trafficFlow.vehicles.contains(head)) head
    else trafficFlow.virtualEnd
  }

  override protected[traffic] def act(timeStep: Double): Unit = {
    if (distance > trafficFlow.length) {
      trafficFlow -= this
      model.actorSystem.stop(actor)
    }
    MovementStrategy(Direction.FORWARD)(timeStep)
  }
}
