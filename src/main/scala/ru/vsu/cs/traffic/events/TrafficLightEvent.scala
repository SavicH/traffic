package ru.vsu.cs.traffic.events

import ru.vsu.cs.traffic.TrafficLight

trait TrafficLightEvent { }

case class ColorChangedEvent(trafficLight: TrafficLight) extends TrafficLightEvent
case class BeforeColorChangedEvent(trafficLight: TrafficLight) extends TrafficLightEvent
