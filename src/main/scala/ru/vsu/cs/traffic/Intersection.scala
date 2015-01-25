package ru.vsu.cs.traffic

import ru.vsu.cs.traffic.Direction.FORWARD

trait Intersection {

  val location: Point

  val trafficFlows: Seq[TrafficFlow]

  val trafficLights: Seq[TrafficLight]

  def apply(flow: TrafficFlow): TrafficLight = trafficLights.find(_(FORWARD) == flow).orNull

}

object Intersection {

  def apply(model: TrafficModel, first: TrafficFlow, second: TrafficFlow): Intersection = {
    new IntersectionImpl(model, first, second)
  }

  private class IntersectionImpl
  (
    private val model: TrafficModel,
    first: TrafficFlow,
    second: TrafficFlow
  )
  extends Intersection {

    val location = first & second

    val trafficFlows = List(first, first.neighbour, second, second.neighbour) filter (_ != null)

    val trafficLights = {
      createTrafficLights(first, second) ::: createTrafficLights(second, first)
    }

    private def createTrafficLights(first: TrafficFlow, second: TrafficFlow) : List[TrafficLight] = {
      for {
        f <- List(first, first.neighbour) filter (_ != null)
      } yield TrafficLight(model, f, second, this)
    }
  }
}
