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

  def intersection: Intersection

  def opposite: TrafficLight

  def trafficFlows: Seq[TrafficFlow]

  def trafficFlow(direction: Direction): TrafficFlow

  def isControlled: Boolean

  def isEnabled: Boolean

  def setEnabled(enabled: Boolean)

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

  private class TrafficLightImpl
  (private val _trafficFlows: Map[Direction, TrafficFlow],
   private val _intersection: Intersection)
  extends TrafficLight {

    private val _opposite = intersection.trafficLights.filter(_.trafficFlow(BACK) == _trafficFlows(FORWARD)).head

    override def intersection: Intersection = _intersection
    
    override def opposite: TrafficLight = _opposite

    override def isControlled: Boolean = true

    override def trafficFlows: Seq[TrafficFlow] = _trafficFlows.values.toList

    override def isEnabled: Boolean = true

    override def color: Color = Color.GREEN

    override def extendColor(delta: Double): Unit = ???

    override def trafficFlow(direction: Direction): TrafficFlow = _trafficFlows(direction)

    override def setEnabled(enabled: Boolean): Unit = ???
  }
}


