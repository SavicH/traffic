package ru.vsu.cs.traffic

import ru.vsu.cs.traffic.event.VehicleSpawned

import scala.collection.mutable
import scala.util.Random

trait TrafficFlow {

  val start: Point

  val end: Point

  val lanes: Int

  val length: Double = start -- end

  def neighbour: TrafficFlow

  def vehicles: Seq[Vehicle]

  def intersections: Seq[Intersection]

  def trafficLights: Seq[TrafficLight] = intersections.map(_(this))

  private[traffic] def &(other: TrafficFlow): Point

  private[traffic] def &&(other: TrafficFlow): Intersection

  private[traffic] def <>(other: TrafficFlow): Direction = util.line.direction(start, end, other.start, other.end)

  private[traffic] def +=(v: Vehicle): Unit

  private[traffic] def -=(v: Vehicle): Unit

  private val random = new Random(System.nanoTime())

  private[traffic] def randomLane = random.nextInt(lanes) + 1

  private[traffic] def act(timeStep: Double)

  protected def ++=(intersection: Intersection): Unit
}

object TrafficFlow {
  def apply(model: TrafficModel, start: Point, end: Point, lanes: Int, isOneWay: Boolean, probability: Probability): TrafficFlow = {
//    TypedActor(model.actorSystem).typedActorOf(TypedProps(classOf[TrafficFlow],
//      new TrafficFlowImpl(model, start, end, lanes, isOneWay, probability)))
    new TrafficFlowImpl(model, start, end, lanes, isOneWay, probability)
  }

  private def apply(model: TrafficModel, start: Point, end: Point, lanes: Int, probability: Probability, neighbour: TrafficFlow): TrafficFlow = {
//    TypedActor(model.actorSystem).typedActorOf(TypedProps(classOf[TrafficFlow],
//      new TrafficFlowImpl(model, start, end, lanes, probability, neighbour)))
    new TrafficFlowImpl(model, start, end, lanes, probability, neighbour)
  }

  private class TrafficFlowImpl
  (
    private val model: TrafficModel,
    val start: Point,
    val end: Point,
    val lanes: Int,
    _isOneWay: Boolean,
    private val probability: Probability
    ) extends TrafficFlow {

    private var _vehicles = new mutable.HashSet[Vehicle]() //todo: WTF

    //private var _neighbour: TrafficFlow = if (_isOneWay) null else TrafficFlow(model, end, start, lanes, probability, TypedActor.self[TrafficFlow])
    private var _neighbour: TrafficFlow = if (_isOneWay) null else TrafficFlow(model, end, start, lanes, probability, this)

    private var _intersections = mutable.MutableList[Intersection]()

    def this(model: TrafficModel, start: Point, end: Point, lanes: Int, probability: Probability, neighbour: TrafficFlow) = {
      this(model, start, end, lanes, _isOneWay = true, probability) //isOneWay = true to prevent recursion
      _neighbour = neighbour
    }

    def neighbour = _neighbour

    override def vehicles: Seq[Vehicle] = _vehicles.toList

    override def intersections: Seq[Intersection] = _intersections.toList

    override protected def ++=(intersection: Intersection): Unit = {
      _intersections += intersection
    }

    override private[traffic] def &(other: TrafficFlow): Point = {
      util.line.intersection(start, end, other.start, other.end)
    }

    override private[traffic] def &&(other: TrafficFlow): Intersection = {
      val point = this & other
      if (point == null) null
      else {
        //val intersection = Intersection(model, TypedActor.self, other)
        val intersection = Intersection(model, this, other)
        this ++= intersection
        other ++= intersection
        if (this.neighbour != null) this.neighbour ++= intersection
        if (other.neighbour != null) other.neighbour ++= intersection
        intersection
      }
    }

    override private[traffic] def +=(v: Vehicle): Unit = _vehicles += v

    override private[traffic] def -=(v: Vehicle): Unit = _vehicles -= v

    private val VehicleSpawnMinDelay = 2.0
    private var vehicleSpawnDelay = VehicleSpawnMinDelay
    private var time = 0.0

    override def act(timeStep: Double) = {
      if (vehicleSpawnDelay >= VehicleSpawnMinDelay) {
        if (math.random < probability(time) * timeStep) {
          for (i <- 1 to math.ceil(probability(time) * timeStep).toInt) {
            val vehicle = Vehicle(model, this, randomLane)
            _vehicles += vehicle
            model.fireVehicleEvent(VehicleSpawned(vehicle))
          }
          vehicleSpawnDelay = 0
        }
      } else {
        vehicleSpawnDelay += timeStep
      }
      time += timeStep
      _vehicles.foreach(_.act(timeStep))
    }

    override def toString: String = {
      "TrafficFlow: " + start + " " + end
    }
  }

}

