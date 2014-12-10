package ru.vsu.cs.traffic

trait TrafficLight {

  sealed trait Color

  object Color {
    case object RED extends Color
    case object YELLOW extends Color
    case object GREEN extends Color
    case object NONE extends Color
  }

  def trafficFlows: Seq[TrafficFlow]

  def trafficFLow(direction: Direction)

  def isControlled: Boolean

  def isEnabled: Boolean

  def setEnabled(enabled: Boolean)

  def color: Color

  def extendColor(delta: Double)

}
