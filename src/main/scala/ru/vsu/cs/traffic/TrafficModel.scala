package ru.vsu.cs.traffic

trait TrafficModel {

  def run()

  def isRunning: Boolean

  def trafficFlows: Seq[TrafficFlow]

  def intersections: Seq[Intersection]

  def trafficLights: Seq[TrafficLight]

  def vehicles: Seq[Vehicle]

  def addTrafficFlow(start: Point, end: Point, lanes: Int, probability: Double, isOneWay: Boolean = false): TrafficModel

}
