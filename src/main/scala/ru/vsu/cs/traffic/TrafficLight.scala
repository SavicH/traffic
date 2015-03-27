package ru.vsu.cs.traffic

import ru.vsu.cs.traffic.Direction._
import ru.vsu.cs.traffic.event.{BeforeColorChanged, ColorChanged}

sealed trait Color extends Serializable with Product

object Color {
  case object RED extends Color
  case object YELLOW extends Color
  case object GREEN extends Color
  case object NONE extends Color
}

trait TrafficLight extends TrafficActor {

  val intersection: Intersection

  val location = intersection.location

  val distance = location -- this(FORWARD).start

  val opposite: TrafficLight

  val trafficFlows: Seq[TrafficFlow]

  def apply(direction: Direction): TrafficFlow

  var durations: Map[Color, Double]

  var turnProbabilities: Map[Direction, Double]

  var color: Color

  def extendColor(delta: Double)

  def time: Double

}

object TrafficLight {

  import ru.vsu.cs.traffic.Color._

  def apply(model: TrafficModel, trafficFlows: Map[Direction, TrafficFlow], intersection: Intersection, color: Color): TrafficLight = {
    new TrafficLightImpl(trafficFlows, intersection, color, model)
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
    new TrafficLightImpl(flows, intersection, color, model)
  }

  private class TrafficLightImpl (
    private val _trafficFlows: Map[Direction, TrafficFlow],
    val intersection: Intersection,
    var color: Color,
    m: TrafficModel)
  extends TrafficLight {

    var durations: Map[Color, Double] = Map(Color.GREEN -> 60, Color.RED -> 60, Color.YELLOW -> 2)

    var turnProbabilities: Map[Direction, Double] = Map(FORWARD -> 0.5, RIGHT -> 0.2, LEFT -> 0.2, BACK -> 0.1)

    private val nextColor = Map(GREEN -> RED, RED -> GREEN)

    lazy val opposite = intersection.trafficLights.find(this(BACK) == _(FORWARD)).orNull

    val trafficFlows: Seq[TrafficFlow] = _trafficFlows.values.toList

    override def apply(direction: Direction): TrafficFlow = _trafficFlows(direction)

    var currentDuration = 0.0

    override def extendColor(delta: Double): Unit = {
      currentDuration -= delta
      isChangingColorEventFired = false
    }

    override def time = durations.getOrElse(color, 0.0) - currentDuration

    val TimeToFireColorChangingEvent = 1.0
    var isChangingColorEventFired = false

    override private[traffic] val model: TrafficModel = m

    override protected def onReceive(message: Any): Unit = message match {
      case Time(timeStep) => act(timeStep)
    }

    override private[traffic] def act(timeStep: Double): Unit = {
      currentDuration += timeStep
      val duration = durations.getOrElse(color, 0.0)
      if (!isChangingColorEventFired && currentDuration > duration - TimeToFireColorChangingEvent) {
        isChangingColorEventFired = true
        model ! BeforeColorChanged(this)
      }
      if (currentDuration > duration) {
        currentDuration = currentDuration - duration
        color = nextColor(color)
        model ! ColorChanged(this)
        isChangingColorEventFired = false
      }
    }
  }
}


