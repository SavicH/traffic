package ru.vsu.cs.traffic

import java.util.{Timer, TimerTask}

import akka.actor.ActorSystem
import ru.vsu.cs.traffic.event.{ModelActed, TrafficLightEvent, TrafficModelEvent, VehicleEvent}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait TrafficModel {

  val DefaultSpawnProbability: Probability = _ => 0.2

  private[traffic] val actorSystem: ActorSystem = ActorSystem()

  def run()

  def run(time: Double): Unit

  def stop()

  def isRunning: Boolean

  def trafficFlows: Seq[TrafficFlow]

  def intersections: Seq[Intersection]

  def trafficLights: Seq[TrafficLight]

  def vehicles: Seq[Vehicle]

  def addFlow(start: Point, end: Point, lanes: Int, probability: Probability = DefaultSpawnProbability, isOneWay: Boolean = false): TrafficModel

  def +=(flow: TrafficFlow): TrafficModel

  type VehicleEventHandler = VehicleEvent => Unit

  val vehicleEventHandlers = ListBuffer[VehicleEventHandler]()

  type TrafficLightEventHandler = TrafficLightEvent => Unit

  val trafficLightEventHandlers = ListBuffer[TrafficLightEventHandler]()

  type TrafficModelEventHandler = TrafficModelEvent => Unit

  val trafficModelEventHandlers = ListBuffer[TrafficModelEventHandler]()

  private[traffic] def fireVehicleEvent(event: VehicleEvent) = vehicleEventHandlers.foreach(_(event))

  private[traffic] def fireTrafficLightEvent(event: TrafficLightEvent) = trafficLightEventHandlers.foreach(_(event))
}

object TrafficModel {

  def apply(): TrafficModel = new TrafficModelImpl()

  def apply(timeStep: Double): TrafficModel = new TrafficModelImpl(timeStep)

  private class TrafficModelImpl (val timeStep: Double = 0.025)extends TrafficModel {

    private val _trafficFlows = mutable.MutableList[TrafficFlow]()

    private val _intersections = mutable.MutableList[Intersection]()

    private var _isRunning = false

    private val timer = new Timer()

    override def run() {
      if (_isRunning) throw new IllegalStateException("Model is already running")
      _isRunning = true
      timer.scheduleAtFixedRate(new TimerTask {
        override def run(): Unit = {
          if (_isRunning) {
            act()
          }
        }
      }, 0, (timeStep * 1000).toInt)
    }

    private def act(): Unit = {
      trafficFlows.foreach(_.act(timeStep))
      trafficLights.foreach(_.act(timeStep))
      trafficModelEventHandlers.foreach(_(ModelActed(currentTime)))
      currentTime += timeStep
    }

    var currentTime = 0.0

    override def run(time: Double): Unit = {
      if (_isRunning) throw new IllegalStateException("Model is already running")
      _isRunning = true
      while (currentTime < time && _isRunning) {
        act()
      }
      _isRunning = false
      currentTime = 0
    }

    override def stop(): Unit = {
      _isRunning = false
      timer.cancel()
    }

    private def addIntersections(flow: TrafficFlow, isOneWay: Boolean) = {
      for {
        otherFlow <- _trafficFlows
        point =  flow & otherFlow
        if point != null
        if !(intersections map (_.location) contains point)
      } _intersections += flow && otherFlow
    }

    override def addFlow(start: Point, end: Point, lanes: Int, probability: Probability, isOneWay: Boolean): TrafficModel = {
      if (lanes <= 0) throw new IllegalArgumentException("Lanes count must be positive")
      if (_isRunning) throw new IllegalStateException("Model is already running")
      val flow = TrafficFlow(this, start, end, lanes, isOneWay, probability)
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

