package ru.vsu.cs.traffic

trait TrafficFlow {

  def start: Point

  def end: Point

  def lanes: Int

  def vehicles: Seq[Vehicle]

  def neighbour: TrafficFlow

  def length: Double = start distance end

  def spawn: TrafficFlow
}
