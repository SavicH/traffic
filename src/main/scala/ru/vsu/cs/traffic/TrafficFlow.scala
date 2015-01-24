package ru.vsu.cs.traffic

import ru.vsu.cs.traffic.util.line

import scala.collection.mutable

trait TrafficFlow {

  def start: Point

  def end: Point

  def lanes: Int

  def vehicles: Seq[Vehicle]

  def neighbour: TrafficFlow

  def length: Double = start -- end

  private[traffic] def &(other: TrafficFlow): Point = line.intersection(start, end, other.start, other.end)

  private[traffic] def <>(other: TrafficFlow): Direction = line.direction(start, end, other.start, other.end)

  private[traffic] def spawn()
}

object TrafficFlow {
  def apply(start: Point, end: Point, lanes: Int, isOneWay: Boolean, probability: Double): TrafficFlow =
    new TrafficFlowImpl(start, end, lanes, isOneWay, probability)

  private class TrafficFlowImpl
   (private val _start: Point,
    private val _end: Point,
    private val _lanes: Int,
    _isOneWay: Boolean,
    private val probability: Double) extends TrafficFlow {

    var _vehicles = mutable.MutableList[Vehicle]()

    var _neighbour: TrafficFlow = if (_isOneWay) null else new TrafficFlowImpl(_end, _start, lanes, probability, this)

    private def this(start: Point, end: Point, lanes: Int, probability: Double, neighbour: TrafficFlow) = {
      this(start, end, lanes, _isOneWay = true, probability) //isOneWay = true to prevent recursion
      _neighbour = neighbour
    }

    override def start: Point = _start

    override def end: Point = _end

    override def lanes: Int = _lanes

    override def neighbour: TrafficFlow = _neighbour

    override def vehicles: Seq[Vehicle] = _vehicles.toList

    override def spawn() = {
      //todo
    }

  }

}

