package ru.vsu.cs.traffic

trait Intersection {

  def location: Point

  def trafficFlows: Seq[TrafficFlow]

  def trafficLights: Seq[TrafficLight]

}
