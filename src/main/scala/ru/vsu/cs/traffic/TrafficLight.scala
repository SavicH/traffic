package ru.vsu.cs.traffic

import ru.vsu.cs.traffic.Direction._

sealed trait Color extends Serializable with Product

object Color {
  case object RED extends Color
  case object YELLOW extends Color
  case object GREEN extends Color
  case object NONE extends Color
}

trait TrafficLight extends TrafficActor {

  import Color._

  val nextColor = Map(GREEN -> YELLOW, YELLOW -> RED, RED -> GREEN)

  val intersection: Intersection

  val opposite: TrafficLight

  val trafficFlows: Seq[TrafficFlow]

  def apply(direction: Direction): TrafficFlow

  var isEnabled: Boolean = true

  var durations: Map[Color, Double]

  var color: Color

  def extendColor(delta: Double)

}

object TrafficLight {

  def apply(model: TrafficModel, trafficFlows: Map[Direction, TrafficFlow], intersection: Intersection, color: Color): TrafficLight = {
    new TrafficLightImpl(trafficFlows, intersection, color)
  }

  def apply(model: TrafficModel, first: TrafficFlow, second: TrafficFlow, intersection: Intersection, color: Color): TrafficLight = {
    val d1 = first <> second
    val d2 = if (d1 == RIGHT) LEFT else RIGHT
    val flows = Map(
      FORWARD -> first,
      BACK -> first.neighbour,
      d1 -> second,
      d2 -> second.neighbour
    )
    new TrafficLightImpl(flows, intersection, color) //todo: actor
  }

  private class TrafficLightImpl (
    private val _trafficFlows: Map[Direction, TrafficFlow],
    val intersection: Intersection,
    var color: Color)
  extends TrafficLight {

    var durations: Map[Color, Double] = Map(Color.GREEN -> 30, Color.RED -> 30, Color.YELLOW -> 2)

    lazy val opposite = intersection.trafficLights.find(this(BACK) == _(FORWARD)).orNull

    val trafficFlows: Seq[TrafficFlow] = _trafficFlows.values.toList

    override def apply(direction: Direction): TrafficFlow = _trafficFlows(direction)

    var currentDuration = 0.0

    override def extendColor(delta: Double): Unit = currentDuration += delta

    override private[traffic] def act(timeStep: Double): Unit = {
      currentDuration += timeStep
      if (currentDuration > durations(color)) {
        currentDuration = currentDuration - durations(color)
        color = nextColor(color)
      }
    }
  }
}


