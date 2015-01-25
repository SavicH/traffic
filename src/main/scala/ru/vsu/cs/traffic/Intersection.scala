package ru.vsu.cs.traffic

trait Intersection {

  val location: Point

  val trafficFlows: Seq[TrafficFlow]

  val trafficLights: Seq[TrafficLight]

}

object Intersection {

  def apply(first: TrafficFlow, second: TrafficFlow): Intersection = new IntersectionImpl(first, second)

  private class IntersectionImpl(first: TrafficFlow, second: TrafficFlow)
  extends Intersection {

    val location = first & second

    val trafficFlows = List(first, first.neighbour, second, second.neighbour) filter (_ != null)

    val trafficLights = {
      createTrafficLights(first, second) ::: createTrafficLights(second, first)
    }

    private def createTrafficLights(first: TrafficFlow, second: TrafficFlow) : List[TrafficLight] = {
      for {
        f <- List(first, first.neighbour) filter (_ != null)
      } yield TrafficLight(f, second, this)
    }
  }
}
