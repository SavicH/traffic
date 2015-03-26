package ru.vsu.cs.traffic.event

trait TrafficModelEvent extends TrafficEvent

case class ModelActed(time: Double) extends TrafficModelEvent

case class ModelStopped() extends TrafficModelEvent
