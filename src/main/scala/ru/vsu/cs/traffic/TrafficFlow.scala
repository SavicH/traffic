package ru.vsu.cs.traffic

import scala.collection.mutable

trait TrafficFlow {

  def start: Point

  def end: Point

  def lanes: Int

  def vehicles: Seq[Vehicle]

  def neighbour: TrafficFlow

  def length: Double = start -- end

  def spawn()
}

object TrafficFlow {
  def apply(start: Point, end: Point, lanes: Int, probability: Double): TrafficFlow =
    new TrafficFlowImpl(start, end, lanes, probability)

  private class TrafficFlowImpl
   (private val _start: Point,
    private val _end: Point,
    private val _lanes: Int,
    private val probability: Double) extends TrafficFlow {

    var _vehicles = mutable.MutableList[Vehicle]()

    var _neighbour: TrafficFlow = null

    private def this(start: Point, end: Point, lanes: Int, probability: Double, neighbour: TrafficFlow) = {
      this(start, end, lanes, probability)
      _neighbour = neighbour
    }

    def createNeighbour: TrafficFlow = {
      if (_neighbour != null) {
        throw new IllegalStateException("Neighbour can be created only once")
      }
      _neighbour = new TrafficFlowImpl(_end, _start, lanes, probability, this)
      _neighbour
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

