package ru.vsu.cs.traffic.vehicle

import ru.vsu.cs.traffic.Color._
import ru.vsu.cs.traffic.Direction._
import ru.vsu.cs.traffic._
import ru.vsu.cs.traffic.event._
import ru.vsu.cs.traffic.util._

import scala.math._

class VehicleImpl(private var _trafficFlow: TrafficFlow, m: TrafficModel, l: Int)
  extends MOBILVehicle {

  override private[traffic] val model: TrafficModel = m

  private var _distance = 0.0
  private var _speed = 0.0
  private var _acceleration = 0.0
  private var _lane = 0

  val length = 5.0

  private var _direction: Direction = null
  private var movementStrategy: MovementStrategy = null

  private var nextIntersection: Intersection = null

  private var endOfFlow: VirtualVehicle = null
  private var startOfFlow: VirtualVehicle = null
  private var target: VirtualVehicle = null

  changeTrafficFlow(_trafficFlow)
  _speed = random * desiredSpeed
  _lane = l

  protected def changeTrafficFlow(trafficFlow: TrafficFlow) = {
    model.fireVehicleEvent(TrafficFlowChanged(this, _trafficFlow))
    _trafficFlow -= this
    trafficFlow += this
    _speed = maneuverSpeed
    _acceleration = 0
    _lane = _direction match {
      case RIGHT => trafficFlow.lanes
      case BACK => trafficFlow.lanes
      case _ => _trafficFlow.randomLane
    }
    _distance = if (nextIntersection == null) 0 else nextIntersection(trafficFlow).distance + minimalGap
    nextIntersection = {
      if (trafficFlow.intersections.filter(_(trafficFlow).distance > distance).isEmpty) null
      else
        trafficFlow.intersections
          .filter(_(trafficFlow).distance > distance)
          .reduceLeft((i1, i2) => if (i1(trafficFlow).distance < i2(trafficFlow).distance) i1 else i2)
    }
    _trafficFlow = trafficFlow
    endOfFlow = VirtualVehicle(_trafficFlow, _trafficFlow.end, 1000)
    startOfFlow = VirtualVehicle(_trafficFlow, _trafficFlow.start, -1000)
    _direction = randomDirection
    movementStrategy = MovementStrategy(_direction)
    target = if (_direction == FORWARD) endOfFlow
    else VirtualVehicle(_trafficFlow, nextIntersection.location, -minimalGap)
  }

  def headVehicle(lane: Int = lane): Vehicle = {
    val vehicles = _trafficFlow.vehicles.filter(_.lane == lane) ++
      (target :: _trafficFlow.trafficLights.filter(_.color == RED)
        .map(l => VirtualVehicle(trafficFlow, l.location)).toList)
    val vehiclesMap = vehicles.map(v => (v.distance, v)).toMap
    val distances = vehiclesMap.keys.filter(_ > distance)
    if (distances.isEmpty) endOfFlow else vehiclesMap(distances.min)
  }

  def backVehicle(lane: Int = lane): Vehicle = {
    val vehiclesMap = _trafficFlow.vehicles.filter(_.lane == lane)
      .map(v => (v.distance, v)).toMap
    val distances = vehiclesMap.keys.filter(_ < distance)
    if (distances.isEmpty) startOfFlow else vehiclesMap(distances.max).asInstanceOf[IDMVehicle]
  }

  override protected def onReceive(message: Any): Unit = message match {
    case Time(timeStep) => act(timeStep)
  }

  override protected[traffic] def act(timeStep: Double): Unit = {
    if (_distance > _trafficFlow.length) {
      _trafficFlow -= this
      model.actorSystem.stop(actor)
    }
    movementStrategy(timeStep)
  }

  override def lane: Int = _lane

  override def distance: Double = _distance

  override def speed: Double = _speed

  override def trafficFlow: TrafficFlow = _trafficFlow

  override def acceleration: Double = _acceleration

  override def direction: Direction = _direction

  private def randomDirection = {
    if (nextIntersection == null) FORWARD
    else {
      val directions = nextIntersection(_trafficFlow).turnProbabilities.map(_._1).toSeq
      val probabilities = nextIntersection(_trafficFlow).turnProbabilities
        .map(_._2).scanLeft(0.0)(_ + _).toSeq
      val result = probabilities.filter(_ <= random).max
      directions(probabilities.indexOf(result))
    }
  }

  protected def basicMovement(timeStep: Double): Unit = {
    val newLane = mobil.lane
    if (newLane != lane) {
      model.fireVehicleEvent(LaneChanged(this, _lane))
      _lane = newLane
    }
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

  private def moveForward(timeStep: Double): Unit = {
    if (nextIntersection != null && distance > nextIntersection(_trafficFlow).distance) {
      model.fireVehicleEvent(IntersectionPassed(this, nextIntersection))
      nextIntersection = nextIntersection.next(_trafficFlow)
      _direction = randomDirection
      movementStrategy = MovementStrategy(_direction)
    } else {
      basicMovement(timeStep)
    }
  }

  private def isGreenLight = nextIntersection(_trafficFlow).color == GREEN

  protected var currentTurningTime = 0.0

  private def moveRight(timeStep: Double): Unit = {
    if (abs(distance - target.distance) < length && isGreenLight) {
      currentTurningTime = counter(timeStep, currentTurningTime, timeToTurnRight) {
        changeTrafficFlow(nextIntersection(_trafficFlow)(RIGHT))
      }
    } else {
      basicMovement(timeStep)
    }
  }

  private def roadIsClear(): Boolean = {
    val d = nextIntersection(_trafficFlow.neighbour).distance
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
    if (abs(distance - target.distance) < length && isGreenLight && roadIsClear()) {
      currentTurningTime = counter(timeStep, currentTurningTime, timeToTurnLeft) {
        changeTrafficFlow(nextIntersection(_trafficFlow)(_direction))
      }
    } else {
      basicMovement(timeStep)
    }
  }

  type MovementStrategy = Double => Unit

  private object MovementStrategy {
    private val strategies: Map[Direction, MovementStrategy] = Map(
      FORWARD -> moveForward,
      LEFT -> moveLeftAndBack,
      RIGHT -> moveRight,
      BACK -> moveLeftAndBack
    )

    def apply(direction: Direction) = strategies(direction)
  }

}

