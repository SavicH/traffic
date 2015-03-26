package ru.vsu.cs.traffic

import java.util.concurrent.TimeUnit

import akka.actor.{ActorSystem, Cancellable}
import ru.vsu.cs.traffic.event._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.Duration

trait TrafficModel extends TrafficActor {

  val DefaultSpawnProbability: Probability = _ => 0.2

  private[traffic] val actorSystem: ActorSystem = ActorSystem()

  def run()

  def run(time: Double): Unit

  def asyncRun(time: Double): Unit

  def stop()

  def isRunning: Boolean

  def trafficFlows: Seq[TrafficFlow]

  def intersections: Seq[Intersection]

  def trafficLights: Seq[TrafficLight]

  def vehicles: Seq[Vehicle]

  def currentTime: Double

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

  private class TrafficModelImpl(val timeStep: Double = 0.025)
    extends TrafficModel {

    private[traffic] val model = this

    private val _trafficFlows = mutable.MutableList[TrafficFlow]()

    private val _intersections = mutable.MutableList[Intersection]()

    private var _isRunning = false

    var _currentTime = 0.0
    var _doneCount = 0
    var _isRealTime = true
    var _vehiclesCount = 0

    override protected def onReceive(message: Any): Unit = message match {
      case Time(step) =>
        act(step)
      case Done() =>
        _doneCount += 1
        if (_doneCount >= _vehiclesCount) {
          trafficModelEventHandlers.foreach(_(ModelActed(_currentTime)))
          _currentTime += timeStep
          _doneCount = 0
          if (!_isRealTime) {
            if (_currentTime < time) {
              act(timeStep)
            } else {
              _isRunning = false
              trafficModelEventHandlers.foreach(_(ModelStopped()))
            }
          }
          _vehiclesCount = vehicles.length
        }
    }

    override private[traffic] def act(timeStep: Double): Unit = {
      for (f <- _trafficFlows) {
        f.actor ! Time(timeStep)
      }
      for (tl <- trafficLights) {
        tl.actor ! Time(timeStep)
      }
    }

    private var actorTask: Cancellable = null

    override def run() {
      if (_isRunning) throw new IllegalStateException("Model is already running")
      _isRunning = true
      _isRealTime = true
      import actorSystem.dispatcher
      actorTask = actorSystem.scheduler.schedule(
        Duration.Zero,
        Duration.create((timeStep * 1000).toInt, TimeUnit.MILLISECONDS),
        actor,
        Time(timeStep)
      )
    }

    private def act(): Unit = {
      trafficFlows.foreach(_.act(timeStep))
      trafficLights.foreach(_.act(timeStep))
      trafficModelEventHandlers.foreach(_(ModelActed(_currentTime)))
      _currentTime += timeStep
    }

    override def run(time: Double): Unit = {
      if (_isRunning) throw new IllegalStateException("Model is already running")
      _isRunning = true
      _isRealTime = true
      while (_currentTime < time && _isRunning) {
        act()
      }
      _isRunning = false
      _currentTime = 0
    }

    var time = 0.0

    override def asyncRun(time: Double): Unit = {
      if (_isRunning) throw new IllegalStateException("Model is already running")
      _isRunning = true
      _isRealTime = true
      this.time = time
      act(timeStep)
    }

    override def stop(): Unit = {
      _isRunning = false
      if (actorTask != null) {
        actorTask.cancel()
      }
    }

    private def addIntersections(flow: TrafficFlow, isOneWay: Boolean) = {
      for {
        otherFlow <- _trafficFlows
        point = flow & otherFlow
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

    override def currentTime: Double = _currentTime

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

