package ru.vsu.cs.traffic.vehicle

import ru.vsu.cs.traffic.Color.{GREEN, RED}
import ru.vsu.cs.traffic.Direction.{BACK, FORWARD, LEFT, RIGHT}
import ru.vsu.cs.traffic._
import ru.vsu.cs.traffic.event._
import ru.vsu.cs.traffic.util._

import scala.math._

class IDMVehicleImpl(protected var _trafficFlow: TrafficFlow, m: TrafficModel, l: Int)
  extends IDMVehicle {

  override private[traffic] val model: TrafficModel = m

  protected var _distance = 0.0
  protected var _speed = 0.0
  protected var _acceleration = 0.0
  protected var _lane = 0

  val length = 5.0

  protected var _direction: Direction = null
  protected var _movementStrategy: MovementStrategy = null
  protected var _nextIntersection: Intersection = null
  protected var _target: VirtualVehicle = null

  changeTrafficFlow(_trafficFlow)
  _speed = random * desiredSpeed
  _lane = l

  protected def changeTrafficFlow(trafficFlow: TrafficFlow) = {
    val oldFlow = _trafficFlow
    _trafficFlow = trafficFlow
    model.fireVehicleEvent(TrafficFlowChanged(this, oldFlow, oldFlow <> _trafficFlow))
    oldFlow -= this
    _trafficFlow += this
    _speed = maneuverSpeed
    _acceleration = 0
    _lane = getLane
    _distance = if (_nextIntersection == null) 0 else _nextIntersection(trafficFlow).distance + minimalGap
    _nextIntersection = getNextIntersection
    _direction = randomDirection
    _movementStrategy = MovementStrategy(_direction)
    _target = if (_direction == FORWARD) trafficFlow.virtualEnd
    else VirtualVehicle(_trafficFlow, _nextIntersection.location, -minimalGap)
  }

  private def getNextIntersection = {
    if (trafficFlow.intersections.filter(_(trafficFlow).distance > distance).isEmpty) null
    else
      trafficFlow.intersections
        .filter(_(trafficFlow).distance > distance)
        .reduceLeft((i1, i2) => if (i1(trafficFlow).distance < i2(trafficFlow).distance) i1 else i2)
  }

  private def getLane = _direction match {
    case RIGHT => trafficFlow.lanes
    case BACK => trafficFlow.lanes
    case _ => _trafficFlow.randomLane
  }

  override private[vehicle] def headVehicle(lane: Int = lane): Vehicle = {
    val vehicles = _trafficFlow.vehicles.filter(_.lane == lane) ++
      (_target :: _trafficFlow.trafficLights.filter(_.color == RED)
        .map(l => VirtualVehicle(trafficFlow, l.location)).toList)
    val vehiclesMap = vehicles.map(v => (v.distance, v)).toMap
    val distances = vehiclesMap.keys.filter(_ > distance)
    if (distances.isEmpty) trafficFlow.virtualEnd else vehiclesMap(distances.min)
  }

  override protected def onReceive(message: Any): Unit = message match {
    case Time(timeStep) =>
      act(timeStep)
      model.actor ! Done()
  }

  override protected[traffic] def act(timeStep: Double): Unit = {
    if (_distance > _trafficFlow.length) {
      _trafficFlow -= this
      model.actorSystem.stop(actor)
    }
    _movementStrategy(timeStep)
  }

  override def lane: Int = _lane

  override def distance: Double = _distance

  override def speed: Double = _speed

  override def trafficFlow: TrafficFlow = _trafficFlow

  override def acceleration: Double = _acceleration

  override def direction: Direction = _direction

  protected def randomDirection = {
    if (_nextIntersection == null) FORWARD
    else {
      val directions = _nextIntersection(_trafficFlow).turnProbabilities.map(_._1).toSeq
      val probabilities = _nextIntersection(_trafficFlow).turnProbabilities
        .map(_._2).scanLeft(0.0)(_ + _).toSeq
      val result = probabilities.filter(_ <= random).max
      directions(probabilities.indexOf(result))
    }
  }

  protected def basicMovement(timeStep: Double): Unit = {
    val oldSpeed = _speed
    _acceleration = idm.acceleration
    _speed += _acceleration * timeStep
    if (_speed < 0) {
      _speed = 0
      _distance -= 0.5 * pow(_speed, 2) * _acceleration
    } else {
      _distance += _speed * timeStep + 0.5 * _acceleration * pow(timeStep, 2)
    }
    val minimalSpeed = 0.1
    if (oldSpeed > minimalSpeed && speed <= minimalSpeed) {
      model.fireVehicleEvent(VehicleStopped(this))
    }
    if (oldSpeed < minimalSpeed && speed >= minimalSpeed) {
      model.fireVehicleEvent(VehicleMoved(this))
    }
  }

  protected def moveForward(timeStep: Double): Unit = {
    if (_nextIntersection != null && distance > _nextIntersection(_trafficFlow).distance) {
      model.fireVehicleEvent(IntersectionPassed(this, _nextIntersection))
      _nextIntersection = _nextIntersection.next(_trafficFlow)
      _direction = randomDirection
      _movementStrategy = MovementStrategy(_direction)
    } else {
      basicMovement(timeStep)
    }
  }

  private def isGreenLight = _nextIntersection(_trafficFlow).color == GREEN

  protected var currentTurningTime = 0.0

  protected def moveRight(timeStep: Double): Unit = {
    if (abs(distance - _target.distance) < length && isGreenLight) {
      currentTurningTime = counter(timeStep, currentTurningTime, timeToTurnRight) {
        changeTrafficFlow(_nextIntersection(_trafficFlow)(RIGHT))
      }
    } else {
      basicMovement(timeStep)
    }
  }

  protected def roadIsClear(): Boolean = {
    val d = _nextIntersection(_trafficFlow.neighbour).distance
    val safeDistance = 100
    val forwardVehicles = _trafficFlow.neighbour.vehicles
      .filter(v =>
      v.direction == FORWARD &&
        v.speed > 0.1 &&
        v.distance > d - safeDistance &&
        v.distance < d)
    if (forwardVehicles.isEmpty) true
    else safeDistance / forwardVehicles
      .reduceLeft((v1, v2) => if (v1.distance > v2.distance) v1 else v2).speed > timeToTurnLeft
  }

  private def moveLeftAndBack(timeStep: Double) = {
    if (abs(distance - _target.distance) < length && isGreenLight && roadIsClear()) {
      currentTurningTime = counter(timeStep, currentTurningTime, timeToTurnLeft) {
        changeTrafficFlow(_nextIntersection(_trafficFlow)(_direction))
      }
    } else {
      basicMovement(timeStep)
    }
  }

  protected type MovementStrategy = Double => Unit

  protected object MovementStrategy {
    private val strategies: Map[Direction, MovementStrategy] = Map(
      FORWARD -> moveForward,
      LEFT -> moveLeftAndBack,
      RIGHT -> moveRight,
      BACK -> moveLeftAndBack
    )

    def apply(direction: Direction) = strategies(direction)
  }

}
