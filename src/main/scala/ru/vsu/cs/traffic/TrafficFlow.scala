package ru.vsu.cs.traffic

import ru.vsu.cs.traffic.util.line

import scala.collection.mutable

trait TrafficFlow {

  val start: Point

  val end: Point

  val lanes: Int

  val length: Double = start -- end

  def neighbour: TrafficFlow

  def vehicles: Seq[Vehicle]

  def intersections: Seq[Intersection]

  private[traffic] def &(other: TrafficFlow): Point = line.intersection(start, end, other.start, other.end)

  private[traffic] def &&(other: TrafficFlow): Intersection

  private[traffic] def <>(other: TrafficFlow): Direction = line.direction(start, end, other.start, other.end)

  private[traffic] def +=(v: Vehicle): Unit

  private[traffic] def -=(v: Vehicle): Unit

  private[traffic] def spawn(timestep: Double)
}

object TrafficFlow {
  def apply(model: TrafficModel, start: Point, end: Point, lanes: Int, isOneWay: Boolean, probability: Double): TrafficFlow =
    new TrafficFlowImpl(model, start, end, lanes, isOneWay, probability)

  private class TrafficFlowImpl
  (
      private val model: TrafficModel,
      val start: Point,
      val end: Point,
      val lanes: Int,
      _isOneWay: Boolean,
      private val probability: Double
  ) extends TrafficFlow {

    var _vehicles = mutable.ListBuffer[Vehicle]()

    private var _neighbour: TrafficFlow = if (_isOneWay) null else new TrafficFlowImpl(model, end, start, lanes, probability, this)

    var _intersections = mutable.MutableList[Intersection]()

    private def this (model: TrafficModel, start: Point, end: Point, lanes: Int, probability: Double, neighbour: TrafficFlow) = {
      this(model, start, end, lanes, _isOneWay = true, probability) //isOneWay = true to prevent recursion
      _neighbour = neighbour
    }

    def neighbour = _neighbour

    override def vehicles: Seq[Vehicle] = _vehicles.toList

    override def intersections: Seq[Intersection] = _intersections.toList

    override private[traffic] def &&(other: TrafficFlow): Intersection = other match {
      case other: TrafficFlowImpl => {
        val point = this & other
        if (point == null) null
        else {
          val intersection = Intersection(model, this, other)
          _intersections += intersection
          other._intersections += intersection
          intersection
        }
      }
      case _ => throw new IllegalArgumentException
    }

    override private[traffic] def +=(v: Vehicle): Unit = _vehicles += v

    override private[traffic] def -=(v: Vehicle): Unit = _vehicles -= v

    override def spawn(timestep: Double) = {
      ??? //todo
    }

  }

}

