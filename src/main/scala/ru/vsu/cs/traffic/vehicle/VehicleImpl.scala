package ru.vsu.cs.traffic.vehicle

import ru.vsu.cs.traffic.Color._
import ru.vsu.cs.traffic.Direction._
import ru.vsu.cs.traffic._
import ru.vsu.cs.traffic.event.{IntersectionPassed, LaneChanged, TrafficFlowChanged}

import scala.math._
import scala.util.Random

class VehicleImpl(private var _trafficFlow: TrafficFlow, model: TrafficModel)
  extends MOBILVehicle {

  val self = this

  private var _distance = 0.0
  private var _speed = 0.0
  private var _lane = 0
  private var _acceleration = 0.0

  val length = 5.0

  private var _direction: Direction = null
  private var movementStrategy: MovementStrategy = null

  private var nextIntersection: Intersection = null

  private var endOfFlow: VirtualVehicle = null
  private var startOfFlow: VirtualVehicle = null
  private var target: VirtualVehicle = null

  changeTrafficFlow(_trafficFlow)
  _speed = random * desiredSpeed
  _lane = VehicleImpl.getRandomLane(_trafficFlow.lanes)

  protected def changeTrafficFlow(trafficFlow: TrafficFlow) = {
    model.fireVehicleEvent(TrafficFlowChanged(self, _trafficFlow))
    _trafficFlow -= self
    trafficFlow += self
    _speed = 3
    _acceleration = 0
    _lane = _direction match {
      case RIGHT => trafficFlow.lanes
      case BACK => trafficFlow.lanes
      case _ => VehicleImpl.getRandomLane(trafficFlow.lanes)
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
    _direction = getRandomDirection
    movementStrategy = MovementStrategy(_direction)
    target = if (_direction == FORWARD) endOfFlow
    else VirtualVehicle(_trafficFlow, nextIntersection.location, -minimalGap)
  }

  def headVehicle(lane: Int = lane): Vehicle = {
    val vehicles = target :: _trafficFlow.vehicles.filter(_.lane == lane).toList :::
      _trafficFlow.trafficLights.filter(_.color == RED)
        .map(l => VirtualVehicle(trafficFlow, l.location)).toList
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

  override protected[traffic] def act(timeStep: Double): Unit = {
    if (_distance > _trafficFlow.length) {
      _trafficFlow -= self
    }
    movementStrategy(timeStep)
  }

  override def lane: Int = _lane

  override def distance: Double = _distance

  override def speed: Double = _speed

  override def trafficFlow: TrafficFlow = _trafficFlow

  override def acceleration: Double = _acceleration

  override def direction: Direction = _direction

  private def getRandomDirection = {
    if (nextIntersection == null) FORWARD
    else {
      val directions = nextIntersection(_trafficFlow).turnProbabilities.map(_._1).toList
      val probabilities = nextIntersection(_trafficFlow).turnProbabilities
        .map(_._2).scanLeft(0.0)(_ + _).toList
      val result = probabilities.filter(_ <= random).max
      directions(probabilities.indexOf(result))
    }
  }

  protected def basicMovement(timeStep: Double): Unit = {
    val newLane = mobil.lane
    if (newLane != lane) {
      model.fireVehicleEvent(LaneChanged(self, _lane))
      _lane = newLane
    }
    _acceleration = idm.acceleration
    _distance += _speed * timeStep + 0.5 * _acceleration * pow(timeStep, 2)
    _speed += _acceleration * timeStep
  }

  private def moveForward(timeStep: Double): Unit = {
    if (nextIntersection != null && distance > nextIntersection(_trafficFlow).distance) {
      model.fireVehicleEvent(IntersectionPassed(self, nextIntersection))
      nextIntersection = nextIntersection.next(_trafficFlow)
      _direction = getRandomDirection
      movementStrategy = MovementStrategy(_direction)
    } else {
      basicMovement(timeStep)
    }
  }

  private def isGreenLight = nextIntersection(_trafficFlow).color == GREEN

  protected var currentTurningTime = 0.0

  private def moveRight(timeStep: Double): Unit = {
    if (abs(distance - target.distance) < length && isGreenLight) {
      if (currentTurningTime >= timeToTurnRight) {
        changeTrafficFlow(nextIntersection(_trafficFlow)(RIGHT))
        currentTurningTime = 0
      } else {
        currentTurningTime += timeStep
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
    if (abs(distance - target.distance) < length &&
      isGreenLight &&
      roadIsClear()) {
      if (currentTurningTime >= timeToTurnLeft) {
        changeTrafficFlow(nextIntersection(_trafficFlow)(_direction))
        currentTurningTime = 0
      } else {
        currentTurningTime += timeStep
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

object VehicleImpl {
  private val random = new Random(System.nanoTime())

  private def getRandomLane(lanes: Int) = random.nextInt(lanes) + 1
}

