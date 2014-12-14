package ru.vsu.cs.traffic

import scala.collection.mutable

trait TrafficModel {

  def run()

  def isRunning: Boolean

  def trafficFlows: Seq[TrafficFlow]

  def intersections: Seq[Intersection]

  def trafficLights: Seq[TrafficLight]

  def vehicles: Seq[Vehicle]

  def addTrafficFlow(start: Point, end: Point, lanes: Int, probability: Double, isOneWay: Boolean = false): TrafficModel

  def +=(flow: TrafficFlow): TrafficModel
}

object TrafficModel {

  def apply(): TrafficModel = new TrafficModelImpl()

  private class TrafficModelImpl extends TrafficModel {

    private val _trafficFlows = mutable.MutableList[TrafficFlow]()

    private val _intersections = mutable.MutableList[Intersection]()

    private var _isRunning = false

    override def run() {
      _isRunning = true
      throw new Exception("Not implemented") //todo
    }

    private def addIntersections(flow: TrafficFlow, isOneWay: Boolean) = {
      //todo
    }

    override def addTrafficFlow(start: Point, end: Point, lanes: Int, probability: Double, isOneWay: Boolean): TrafficModel = {
      if (lanes <= 0) throw new IllegalArgumentException("Lanes count must be positive")
      if (_isRunning) throw new IllegalStateException("Model is already running")
      val flow = TrafficFlow(start, end, lanes, isOneWay, probability)
      addIntersections(flow, isOneWay)
      this += flow
    }

    override def +=(flow: TrafficFlow) = {
      _trafficFlows += flow
      if (flow.neighbour != null)
        _trafficFlows += flow.neighbour
      this
    }

    override def intersections: Seq[Intersection] = _intersections.toList

    override def isRunning: Boolean = _isRunning

    override def trafficFlows: Seq[TrafficFlow] = _trafficFlows.toList

    override def vehicles: Seq[Vehicle] =
      for {
        flow <- _trafficFlows
        vehicle <- flow.vehicles
      } yield vehicle

    override def trafficLights: Seq[TrafficLight] =
      for {
        intersection <- _intersections
        light <- intersection.trafficLights
      } yield light
  }
}

