package ru.vsu.cs.traffic.vehicles

import scala.util.Random

import ru.vsu.cs.traffic.{Intersection, Direction, Vehicle, TrafficFlow}
import ru.vsu.cs.traffic.Color._
import ru.vsu.cs.traffic.Direction._

import scala.math._

class VehicleImpl(private var _trafficFlow: TrafficFlow)
  extends MOBILVehicle {

  private var _distance = 0.0
  private var _speed = random * desiredSpeed
  private var _lane = VehicleImpl.getRandomLane(_trafficFlow.lanes)
  private var _acceleration = 0.0

  val length = 5.0

  private var direction: Direction = null
  private var movementStrategy: MovementStrategy = null

  private var nextIntersection: Intersection = null

  private var endOfFlow: VirtualVehicle = null
  private var startOfFlow: VirtualVehicle = null

  private def target: VirtualVehicle = if (direction == FORWARD) endOfFlow else VirtualVehicle(_trafficFlow, nextIntersection.location, -minimalGap)

  changeTrafficFlow(_trafficFlow)

  private def changeTrafficFlow(trafficFlow: TrafficFlow) = {
    _trafficFlow -= this
    trafficFlow += this
    _speed = 3
    _acceleration = 0
    _lane = direction match {
      case RIGHT => trafficFlow.lanes
      case BACK => trafficFlow.lanes
      case _ => VehicleImpl.getRandomLane(trafficFlow.lanes)
    }
    _distance = if (nextIntersection == null) 0 else nextIntersection(trafficFlow).distance + minimalGap
    nextIntersection = {
      if (trafficFlow.intersections.isEmpty) null
      else
        trafficFlow.intersections
          .reduceLeft((i1, i2) => if (i1(trafficFlow).distance < i2(trafficFlow).distance) i1 else i2)
    }
    _trafficFlow = trafficFlow
    endOfFlow = VirtualVehicle(_trafficFlow, _trafficFlow.end, 1000)
    startOfFlow = VirtualVehicle(_trafficFlow, _trafficFlow.start, -1000)
    direction = getRandomDirection
    movementStrategy = MovementStrategy(direction)
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

  override private[traffic] def act(timeStep: Double): Unit = {
    if (_distance > _trafficFlow.length) {
      _trafficFlow -= this
    }
    movementStrategy(timeStep)
  }

  override def lane: Int = _lane

  override def distance: Double = _distance

  override def speed: Double = _speed

  override def trafficFlow: TrafficFlow = _trafficFlow

  override def acceleration: Double = _acceleration

  private def getRandomDirection = {
    if (nextIntersection == null) FORWARD
    else {
      if (random > .5) FORWARD else RIGHT //todo
    }
  }

  private def basicMovement(timeStep: Double): Unit = {
    _lane = mobil.lane
    _acceleration = idm.acceleration
    _distance += _speed * timeStep + 0.5 * _acceleration * pow(timeStep, 2)
    _speed += _acceleration * timeStep
  }

  private def moveForward(timeStep: Double): Unit = {
    if (nextIntersection != null && _distance > nextIntersection(_trafficFlow).distance) {
      nextIntersection = nextIntersection.next(_trafficFlow)
      direction = getRandomDirection
      movementStrategy = MovementStrategy(direction)
    } else {
      basicMovement(timeStep)
    }
  }

  private def moveRight(timeStep: Double): Unit = {
    if (abs(distance - target.distance) < 2 * minimalGap) {
      println("TURN RIGHT")
      changeTrafficFlow(nextIntersection(_trafficFlow)(RIGHT))
    } else {
      basicMovement(timeStep)
    }
  }

  private def moveLeftAndBack(timeStep: Double) = {
    ???
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

