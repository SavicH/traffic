package ru.vsu.cs.traffic

trait Intersection {

  def location: Point

  def trafficFlows: Seq[TrafficFlow]

  def trafficLights: Seq[TrafficLight]

}

object Intersection {

  def apply(first: TrafficFlow, second: TrafficFlow) = new IntersectionImpl(first, second)

  private class IntersectionImpl
    (first: TrafficFlow, second: TrafficFlow)
  extends Intersection {

    private val _location = first & second
    private val _trafficFlows = List(first, first.neighbour, second, second.neighbour) filter (_ != null)
    private val _trafficLights = {
      createTrafficLights(first, second) ::: createTrafficLights(second, first)
    }

    private def createTrafficLights(first: TrafficFlow, second: TrafficFlow) : List[TrafficLight] = {
      for {
        f <- List(first, first.neighbour) filter (_ != null)
      } yield TrafficLight(f, second, this)
    }

    override def location: Point = _location

    override def trafficFlows: Seq[TrafficFlow] = _trafficFlows

    override def trafficLights: Seq[TrafficLight] = _trafficLights
  }
}
