package ru.vsu.cs.traffic.events

import ru.vsu.cs.traffic.TrafficLight

trait TrafficLightEvent {

}

case class TrafficLightChangedColorEvent(trafficLight: TrafficLight) extends TrafficLightEvent
case class TrafficLightIsChangingColorEvent(trafficLight: TrafficLight) extends TrafficLightEvent
