package ru.vsu.cs.traffic

import java.util.concurrent.TimeUnit

import akka.actor._
import com.typesafe.config.{ConfigFactory, ConfigValueFactory}
import ru.vsu.cs.traffic.event._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.Duration

trait TrafficModel extends TrafficActor {

  val DefaultSpawnProbability: Probability = _ => 0.2

  private var _truckProportion = 0.2

  def truckProportion = _truckProportion

  def truckProportion_=(value: Double) = {
    if ((value < 0) || (value > 1)) {
      throw new IllegalArgumentException("Value must be between 0 and 1")
    }
    _truckProportion = value
  }

  private val config = ConfigFactory.load()
    .withValue("akka.loglevel", ConfigValueFactory.fromAnyRef("OFF"))
    .withValue("akka.stdout-loglevel", ConfigValueFactory.fromAnyRef("OFF"))

  private[traffic] val actorSystem: ActorSystem = ActorSystem("system", config)

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

  def isSimple: Boolean

  def addFlow(start: Point, end: Point, lanes: Int, probability: Probability = DefaultSpawnProbability, isOneWay: Boolean = false, secondProbability: Probability = null): TrafficModel

  def +=(flow: TrafficFlow): TrafficModel

  type VehicleEventHandler = VehicleEvent => Unit

  val vehicleEventHandlers = ListBuffer[VehicleEventHandler]()

  type TrafficLightEventHandler = TrafficLightEvent => Unit

  val trafficLightEventHandlers = ListBuffer[TrafficLightEventHandler]()

  type TrafficModelEventHandler = TrafficModelEvent => Unit

  val trafficModelEventHandlers = ListBuffer[TrafficModelEventHandler]()

  protected def fireVehicleEvent(event: VehicleEvent) = vehicleEventHandlers.foreach(_(event))

  protected def fireTrafficLightEvent(event: TrafficLightEvent) = trafficLightEventHandlers.foreach(_(event))

  protected def fireTrafficModelEvent(event: TrafficModelEvent) = trafficModelEventHandlers.foreach(_(event))
}

object TrafficModel {

  def apply(): TrafficModel = new TrafficModelImpl()

  def apply(timeStep: Double, isSimple: Boolean = false): TrafficModel = new TrafficModelImpl(timeStep, isSimple)

  private class TrafficModelImpl(val timeStep: Double = 0.025, val isSimple: Boolean = false)
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
      case e: TrafficEvent =>
        EventHandler() ! e
      case Time(step) =>
        act(step)
      case Done() =>
        _doneCount += 1
        if (_doneCount >= _vehiclesCount) {
          this ! ModelActed(_currentTime)
          _currentTime += timeStep
          _doneCount = 0
          if (!_isRealTime) {
            if (_currentTime < time) {
              act(timeStep)
            } else {
              _isRunning = false
              this ! ModelStopped()
            }
          }
          _vehiclesCount = vehicles.length
        }
    }

    override private[traffic] def act(timeStep: Double): Unit = {
      _trafficFlows.foreach(_ ! Time(timeStep))
      trafficLights.foreach(_ ! Time(timeStep))
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
      this ! ModelActed(_currentTime)
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

    override def addFlow(start: Point, end: Point, lanes: Int, probability: Probability, isOneWay: Boolean, secondProbability: Probability = null): TrafficModel = {
      if (lanes <= 0) throw new IllegalArgumentException("Lanes count must be positive")
      if (_isRunning) throw new IllegalStateException("Model is already running")
      val flow = TrafficFlow(this, start, end, lanes, isOneWay, probability, secondProbability)
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

    private class EventHandler extends UntypedActor() {
      @throws[Exception](classOf[Exception])
      override def onReceive(message: Any): Unit = message match {
        case e: TrafficModelEvent =>
          fireTrafficModelEvent(e)
          getSelf() ! PoisonPill
        case e: TrafficLightEvent =>
          fireTrafficLightEvent(e)
          getSelf() ! PoisonPill
        case e: VehicleEvent =>
          fireVehicleEvent(e)
          getSelf() ! PoisonPill
      }
    }

    private object EventHandler {
      def apply(): ActorRef = actorSystem.actorOf(Props(new EventHandler()))
    }
  }

}

