package ru.vsu.cs.traffic

import ru.vsu.cs.traffic.Direction._

trait TrafficLight {

  sealed trait Color

  object Color {
    case object RED extends Color
    case object YELLOW extends Color
    case object GREEN extends Color
    case object NONE extends Color
  }

  val intersection: Intersection

  val opposite: TrafficLight

  val trafficFlows: Seq[TrafficFlow]

  def apply(direction: Direction): TrafficFlow

  var isEnabled: Boolean = true

  def color: Color

  def extendColor(delta: Double)

}

object TrafficLight {

  def apply(trafficFlows: Map[Direction, TrafficFlow], intersection: Intersection): TrafficLight = {
    new TrafficLightImpl(trafficFlows, intersection)
  }

  def apply(first: TrafficFlow, second: TrafficFlow, intersection: Intersection): TrafficLight = {
    val d1 = first <> second
    val d2 = if (d1 == RIGHT) LEFT else RIGHT
    val flows = Map(
      FORWARD -> first,
      BACK -> first.neighbour,
      d1 -> second,
      d2 -> second.neighbour
    )
    new TrafficLightImpl(flows, intersection)
  }

  private class TrafficLightImpl (
    private val _trafficFlows: Map[Direction, TrafficFlow],
    val intersection: Intersection)
  extends TrafficLight {

    lazy val opposite = intersection.trafficLights.find(this(BACK) == _(FORWARD)).orNull

    val trafficFlows: Seq[TrafficFlow] = _trafficFlows.values.toList

    override def apply(direction: Direction): TrafficFlow = _trafficFlows(direction)

    override def color: Color = Color.GREEN

    override def extendColor(delta: Double): Unit = ???
  }
}


